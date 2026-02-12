package no_unused_vars

import (
	"context"
	"regexp"
	"strings"

	"github.com/microsoft/typescript-go/shim/ast"
	"github.com/microsoft/typescript-go/shim/checker"
	"github.com/microsoft/typescript-go/shim/compiler"
	"github.com/microsoft/typescript-go/shim/core"
	"github.com/typescript-eslint/tsgolint/internal/rule"
	"github.com/typescript-eslint/tsgolint/internal/utils"
)

func unusedVarMessage(varName string, action string, additional string) rule.RuleMessage {
	suffix := ""
	if additional != "" {
		suffix = ". " + additional
	}
	return rule.RuleMessage{
		Id:          "unusedVar",
		Description: "'" + varName + "' is " + action + " but never used" + suffix + ".",
	}
}

func usedOnlyAsTypeMessage(varName string, action string, additional string) rule.RuleMessage {
	suffix := ""
	if additional != "" {
		suffix = ". " + additional
	}
	return rule.RuleMessage{
		Id:          "usedOnlyAsType",
		Description: "'" + varName + "' is " + action + " but only used as a type" + suffix + ".",
	}
}

func usedIgnoredVarMessage(varName string, additional string) rule.RuleMessage {
	suffix := ""
	if additional != "" {
		suffix = ". " + additional
	}
	return rule.RuleMessage{
		Id:          "usedIgnoredVar",
		Description: "'" + varName + "' is marked as ignored but is used" + suffix + ".",
	}
}

func removeUnusedVarMessage(varName string) rule.RuleMessage {
	return rule.RuleMessage{
		Id:          "removeUnusedVar",
		Description: "Remove unused variable '" + varName + "'.",
	}
}

func removeUnusedImportDeclarationMessage(varName string) rule.RuleMessage {
	return rule.RuleMessage{
		Id:          "removeUnusedImportDeclaration",
		Description: "Remove unused import '" + varName + "'.",
	}
}

type compiledOptions struct {
	vars                          string
	args                          string
	caughtErrors                  string
	ignoreRestSiblings            bool
	ignoreClassWithStaticInitBlock bool
	reportUsedIgnorePattern       bool
	varsIgnorePattern             *regexp.Regexp
	argsIgnorePattern             *regexp.Regexp
	caughtErrorsIgnorePattern     *regexp.Regexp
	destructuredArrayIgnorePattern *regexp.Regexp
}

func compileOptions(opts NoUnusedVarsOptions) compiledOptions {
	co := compiledOptions{
		vars:                           string(opts.Vars),
		args:                           string(opts.Args),
		caughtErrors:                   string(opts.CaughtErrors),
		ignoreRestSiblings:             opts.IgnoreRestSiblings,
		ignoreClassWithStaticInitBlock: opts.IgnoreClassWithStaticInitBlock,
		reportUsedIgnorePattern:        opts.ReportUsedIgnorePattern,
	}
	if co.vars == "" {
		co.vars = "all"
	}
	if co.args == "" {
		co.args = "after-used"
	}
	if co.caughtErrors == "" {
		co.caughtErrors = "all"
	}
	if opts.VarsIgnorePattern != "" {
		co.varsIgnorePattern = regexp.MustCompile(opts.VarsIgnorePattern)
	}
	if opts.ArgsIgnorePattern != "" {
		co.argsIgnorePattern = regexp.MustCompile(opts.ArgsIgnorePattern)
	}
	if opts.CaughtErrorsIgnorePattern != "" {
		co.caughtErrorsIgnorePattern = regexp.MustCompile(opts.CaughtErrorsIgnorePattern)
	}
	if opts.DestructuredArrayIgnorePattern != "" {
		co.destructuredArrayIgnorePattern = regexp.MustCompile(opts.DestructuredArrayIgnorePattern)
	}
	return co
}

var NoUnusedVarsRule = rule.Rule{
	Name: "no-unused-vars",
	Run: func(ctx rule.RuleContext, options any) rule.RuleListeners {
		opts := utils.UnmarshalOptions[NoUnusedVarsOptions](options, "no-unused-vars")
		co := compileOptions(opts)

		return rule.RuleListeners{
			ast.KindEndOfFile: func(node *ast.Node) {
				checkSourceFile(ctx, co, node.Parent)
			},
		}
	},
}

// referenceKinds tracks which meanings (value, type, namespace) of a symbol are referenced.
const (
	refKindValue     ast.SymbolFlags = ast.SymbolFlagsVariable
	refKindType      ast.SymbolFlags = ast.SymbolFlagsType
	refKindNamespace ast.SymbolFlags = ast.SymbolFlagsNamespace
)

// referenceMap tracks which symbols are referenced and with what kinds.
type referenceMap map[*ast.Symbol]ast.SymbolFlags

// buildReferenceMap walks the AST and uses GetSymbolAtLocation to build a map of
// all referenced symbols. This works for both module and script (global) files.
func buildReferenceMap(sourceFileNode *ast.Node, fileChecker *checker.Checker) referenceMap {
	refs := make(referenceMap)

	var walk func(node *ast.Node)
	walk = func(node *ast.Node) {
		if node.Kind == ast.KindIdentifier {
			// Skip identifiers that are declaration names
			if !isDeclarationName(node) {
				sym := fileChecker.GetSymbolAtLocation(node)
				if sym != nil {
					meaning := refKindValue
					if isInTypePosition(node) {
						meaning = refKindType
					}
					refs[sym] |= meaning
				}
			}
		}
		node.ForEachChild(func(child *ast.Node) bool {
			walk(child)
			return false
		})
	}
	walk(sourceFileNode)

	return refs
}

// isDeclarationName returns true if the identifier is the name of a declaration
// (not a reference to something else).
func isDeclarationName(node *ast.Node) bool {
	parent := node.Parent
	if parent == nil {
		return false
	}
	// The node is a declaration name if it's the Name() of its parent declaration
	name := parent.Name()
	if name == node {
		switch parent.Kind {
		case ast.KindVariableDeclaration, ast.KindParameter,
			ast.KindFunctionDeclaration, ast.KindFunctionExpression,
			ast.KindClassDeclaration, ast.KindClassExpression,
			ast.KindInterfaceDeclaration, ast.KindTypeAliasDeclaration,
			ast.KindEnumDeclaration, ast.KindModuleDeclaration,
			ast.KindMethodDeclaration, ast.KindPropertyDeclaration,
			ast.KindGetAccessor, ast.KindSetAccessor,
			ast.KindBindingElement,
			ast.KindImportSpecifier, ast.KindImportClause,
			ast.KindNamespaceImport,
			ast.KindTypeParameter,
			ast.KindEnumMember:
			return true
		}
	}
	// Also treat import specifiers' propertyName as a declaration name (the "as" part)
	if ast.IsImportSpecifier(parent) && parent.AsImportSpecifier().PropertyName == node {
		return true
	}
	// LabeledStatement names
	if parent.Kind == ast.KindLabeledStatement {
		return true
	}
	// Export specifier names
	if ast.IsExportSpecifier(parent) {
		return true
	}
	return false
}

// isInTypePosition returns true if the identifier is used in a type annotation context.
func isInTypePosition(node *ast.Node) bool {
	parent := node.Parent
	for parent != nil {
		switch parent.Kind {
		case ast.KindTypeReference, ast.KindTypeQuery,
			ast.KindIndexedAccessType, ast.KindMappedType,
			ast.KindConditionalType, ast.KindIntersectionType, ast.KindUnionType,
			ast.KindArrayType, ast.KindTupleType, ast.KindOptionalType, ast.KindRestType,
			ast.KindTypeLiteral, ast.KindTypeAliasDeclaration,
			ast.KindInterfaceDeclaration,
			ast.KindFunctionType, ast.KindConstructorType,
			ast.KindTypePredicate, ast.KindTypeOperator,
			ast.KindParenthesizedType, ast.KindInferType,
			ast.KindTemplateLiteralType, ast.KindNamedTupleMember:
			return true
		case ast.KindExpressionWithTypeArguments:
			// Check if this is in an extends/implements clause
			if parent.Parent != nil && parent.Parent.Kind == ast.KindHeritageClause {
				return false // extends clause in class is value position
			}
			return true
		case ast.KindParameter:
			// The type annotation of a parameter is type position, but the name/default are not
			if parent.Type() == node || isAncestorOf(parent.Type(), node) {
				return true
			}
			return false
		case ast.KindVariableDeclaration, ast.KindPropertyDeclaration, ast.KindPropertySignature:
			// Type annotation
			if parent.Type() == node || isAncestorOf(parent.Type(), node) {
				return true
			}
			return false
		case ast.KindFunctionDeclaration, ast.KindFunctionExpression, ast.KindArrowFunction,
			ast.KindMethodDeclaration, ast.KindMethodSignature,
			ast.KindGetAccessor, ast.KindSetAccessor,
			ast.KindConstructor, ast.KindCallSignature, ast.KindConstructSignature:
			// Return type annotation
			if parent.Type() == node || isAncestorOf(parent.Type(), node) {
				return true
			}
			return false
		// Stop walking at statement/expression boundaries
		case ast.KindBlock, ast.KindSourceFile,
			ast.KindExpressionStatement, ast.KindReturnStatement, ast.KindIfStatement,
			ast.KindVariableStatement, ast.KindForStatement, ast.KindForInStatement, ast.KindForOfStatement,
			ast.KindWhileStatement, ast.KindDoStatement, ast.KindSwitchStatement,
			ast.KindCallExpression, ast.KindNewExpression,
			ast.KindBinaryExpression, ast.KindPrefixUnaryExpression, ast.KindPostfixUnaryExpression,
			ast.KindPropertyAccessExpression, ast.KindElementAccessExpression,
			ast.KindConditionalExpression, ast.KindTemplateExpression,
			ast.KindTaggedTemplateExpression, ast.KindJsxExpression,
			ast.KindObjectLiteralExpression, ast.KindArrayLiteralExpression,
			ast.KindSpreadElement, ast.KindParenthesizedExpression,
			ast.KindVoidExpression, ast.KindDeleteExpression, ast.KindTypeOfExpression,
			ast.KindAwaitExpression, ast.KindYieldExpression,
			ast.KindAsExpression, ast.KindSatisfiesExpression, ast.KindNonNullExpression,
			ast.KindClassDeclaration, ast.KindClassExpression:
			return false
		}
		parent = parent.Parent
	}
	return false
}

func isAncestorOf(ancestor *ast.Node, node *ast.Node) bool {
	if ancestor == nil {
		return false
	}
	current := node.Parent
	for current != nil {
		if current == ancestor {
			return true
		}
		current = current.Parent
	}
	return false
}

func checkSourceFile(ctx rule.RuleContext, co compiledOptions, sourceFileNode *ast.Node) {
	sourceFile := ctx.SourceFile

	// Don't check .d.ts files - all declarations are ambient
	if strings.HasSuffix(sourceFile.FileName(), ".d.ts") {
		return
	}

	// Get the checker associated with this source file.
	fileChecker, done := compiler.Program_GetTypeCheckerForFile(ctx.Program, context.Background(), sourceFile)
	defer done()

	// Force type-checking so identifiers are resolved.
	checker.Checker_checkSourceFile(fileChecker, context.Background(), sourceFile)

	// Build reference map by walking the AST and resolving identifiers.
	// This works for both module files (where isReferenced would also work)
	// and script files (where isReferenced doesn't track references because
	// globals are resolved after the SymbolReferenced callback point).
	refs := buildReferenceMap(sourceFileNode, fileChecker)

	// For module files, also incorporate the checker's symbolReferenceLinks
	// which may have additional cross-reference information.
	if ast.IsExternalOrCommonJSModule(sourceFile) {
		for _, symbol := range sourceFileNode.Locals() {
			kinds := checker.Checker_getSymbolReferenceKinds(fileChecker, symbol)
			if kinds != 0 {
				refs[symbol] |= kinds
			}
		}
	}

	// Walk the AST looking for scopes with locals
	var walkScopes func(node *ast.Node)
	walkScopes = func(node *ast.Node) {
		if hasLocals(node) {
			checkUnusedLocals(ctx, co, node, fileChecker, refs)
		}
		node.ForEachChild(func(child *ast.Node) bool {
			walkScopes(child)
			return false
		})
	}
	walkScopes(sourceFileNode)
}

func hasLocals(node *ast.Node) bool {
	switch node.Kind {
	case ast.KindSourceFile, ast.KindModuleDeclaration, ast.KindBlock, ast.KindCaseBlock,
		ast.KindForStatement, ast.KindForInStatement, ast.KindForOfStatement,
		ast.KindCatchClause,
		ast.KindConstructor, ast.KindFunctionExpression, ast.KindFunctionDeclaration,
		ast.KindArrowFunction, ast.KindMethodDeclaration, ast.KindGetAccessor, ast.KindSetAccessor:
		return node.Locals() != nil
	}
	return false
}

func checkUnusedLocals(ctx rule.RuleContext, co compiledOptions, scopeNode *ast.Node, fileChecker *checker.Checker, refs referenceMap) {
	locals := scopeNode.Locals()
	if locals == nil {
		return
	}

	for _, symbol := range locals {
		// Skip internal symbols (names starting with internal prefix)
		if len(symbol.Name) > 0 && symbol.Name[0] == '\xFE' {
			continue
		}

		referenceKinds := refs[symbol]

		// Check if symbol is exported
		if symbol.ExportSymbol != nil {
			continue
		}

		// Handle type parameters specially
		if symbol.Flags&ast.SymbolFlagsTypeParameter != 0 {
			if symbol.Flags&ast.SymbolFlagsVariable == 0 || referenceKinds&refKindValue != 0 {
				continue
			}
		} else if referenceKinds != 0 {
			// Non-type-parameter that has references
			checkUsedWithIgnorePattern(ctx, co, symbol, referenceKinds)
			continue
		}

		// At this point, the symbol is unreferenced (or only type-referenced)
		for _, decl := range symbol.Declarations {
			processUnusedDeclaration(ctx, co, symbol, decl, referenceKinds, scopeNode, fileChecker, refs)
		}
	}
}

func checkUsedWithIgnorePattern(ctx rule.RuleContext, co compiledOptions, symbol *ast.Symbol, referenceKinds ast.SymbolFlags) {
	if !co.reportUsedIgnorePattern {
		return
	}

	name := symbol.Name

	matched := false
	for _, decl := range symbol.Declarations {
		if isParameter(decl) {
			if co.argsIgnorePattern != nil && co.argsIgnorePattern.MatchString(name) {
				matched = true
				break
			}
		} else if isCatchClauseVariable(decl) {
			if co.caughtErrorsIgnorePattern != nil && co.caughtErrorsIgnorePattern.MatchString(name) {
				matched = true
				break
			}
		} else if isArrayDestructure(decl) {
			if co.destructuredArrayIgnorePattern != nil && co.destructuredArrayIgnorePattern.MatchString(name) {
				matched = true
				break
			}
		} else {
			if co.varsIgnorePattern != nil && co.varsIgnorePattern.MatchString(name) {
				matched = true
				break
			}
		}
	}

	if matched {
		reportNode := getReportNode(symbol)
		if reportNode != nil {
			additional := getAdditionalForIgnorePattern(co, symbol)
			ctx.ReportNode(reportNode, usedIgnoredVarMessage(name, additional))
		}
	}
}

func processUnusedDeclaration(ctx rule.RuleContext, co compiledOptions, symbol *ast.Symbol, decl *ast.Node, referenceKinds ast.SymbolFlags, scopeNode *ast.Node, fileChecker *checker.Checker, refs referenceMap) {
	name := symbol.Name
	if name == "" {
		return
	}

	// Skip if in ambient context
	if decl.Flags&ast.NodeFlagsAmbient != 0 {
		return
	}

	// Skip exported declarations
	if isExportedDeclaration(decl) {
		return
	}

	switch {
	case isImportDeclaration(decl):
		processUnusedImport(ctx, co, symbol, decl, referenceKinds)
		return

	case isCatchClauseVariable(decl):
		if co.caughtErrors == "none" {
			return
		}
		if co.caughtErrorsIgnorePattern != nil && co.caughtErrorsIgnorePattern.MatchString(name) {
			return
		}

	case isParameter(decl):
		if co.args == "none" {
			return
		}
		if co.argsIgnorePattern != nil && co.argsIgnorePattern.MatchString(name) {
			return
		}
		if co.args == "after-used" && isParameterUsedAfter(refs, decl) {
			return
		}

	case ast.IsTypeAliasDeclaration(decl) || ast.IsInterfaceDeclaration(decl):
		if co.varsIgnorePattern != nil && co.varsIgnorePattern.MatchString(name) {
			return
		}

	case ast.IsClassDeclaration(decl):
		if co.varsIgnorePattern != nil && co.varsIgnorePattern.MatchString(name) {
			return
		}
		if co.ignoreClassWithStaticInitBlock && classHasStaticBlock(decl) {
			return
		}

	default:
		if co.varsIgnorePattern != nil && co.varsIgnorePattern.MatchString(name) {
			return
		}
		if isArrayDestructure(decl) && co.destructuredArrayIgnorePattern != nil && co.destructuredArrayIgnorePattern.MatchString(name) {
			return
		}
		if co.ignoreRestSiblings && isRestSibling(decl) {
			return
		}
		if isForInOfVariable(decl) && strings.HasPrefix(name, "_") {
			return
		}
		if isUsingDeclaration(decl) && strings.HasPrefix(name, "_") {
			return
		}
	}

	// Check `vars: "local"` - skip module-level
	if co.vars == "local" && scopeNode.Kind == ast.KindSourceFile {
		return
	}

	action := getAction(symbol)

	// Check for type-only usage
	if referenceKinds != 0 && referenceKinds&refKindValue == 0 {
		if !isImportDeclaration(decl) {
			reportNode := getReportNode(symbol)
			if reportNode != nil {
				ctx.ReportNodeWithSuggestions(reportNode, usedOnlyAsTypeMessage(name, action, ""), func() []rule.RuleSuggestion {
					return []rule.RuleSuggestion{{
						Message: removeUnusedVarMessage(name),
					}}
				})
			}
			return
		}
	}

	reportNode := getReportNode(symbol)
	if reportNode == nil {
		return
	}

	if isImportDeclaration(decl) {
		ctx.ReportNodeWithSuggestions(reportNode, unusedVarMessage(name, action, ""), func() []rule.RuleSuggestion {
			return buildImportSuggestions(ctx, symbol, decl, name)
		})
	} else {
		ctx.ReportNode(reportNode, unusedVarMessage(name, action, ""))
	}
}

func processUnusedImport(ctx rule.RuleContext, co compiledOptions, symbol *ast.Symbol, decl *ast.Node, referenceKinds ast.SymbolFlags) {
	name := symbol.Name
	if name == "" {
		return
	}

	if co.varsIgnorePattern != nil && co.varsIgnorePattern.MatchString(name) {
		return
	}

	if co.vars == "local" {
		return
	}

	action := "defined"
	reportNode := getReportNode(symbol)
	if reportNode == nil {
		return
	}

	// Check for type-only usage (import used only as type)
	if referenceKinds != 0 && referenceKinds&refKindValue == 0 {
		return
	}

	ctx.ReportNodeWithSuggestions(reportNode, unusedVarMessage(name, action, ""), func() []rule.RuleSuggestion {
		return buildImportSuggestions(ctx, symbol, decl, name)
	})
}

func buildImportSuggestions(ctx rule.RuleContext, symbol *ast.Symbol, decl *ast.Node, name string) []rule.RuleSuggestion {
	fixes := buildImportRemovalFixes(ctx, decl)
	if len(fixes) == 0 {
		return nil
	}
	return []rule.RuleSuggestion{{
		Message:  removeUnusedImportDeclarationMessage(name),
		FixesArr: fixes,
	}}
}

func buildImportRemovalFixes(ctx rule.RuleContext, decl *ast.Node) []rule.RuleFix {
	importDecl := findImportDeclaration(decl)
	if importDecl == nil {
		return nil
	}

	switch {
	case ast.IsImportSpecifier(decl):
		importClause := importDecl.AsImportDeclaration().ImportClause
		if importClause == nil {
			return nil
		}
		namedBindings := importClause.AsImportClause().NamedBindings
		if namedBindings == nil || !ast.IsNamedImports(namedBindings) {
			return nil
		}

		elements := namedBindings.AsNamedImports().Elements.Nodes
		if len(elements) == 1 {
			if importClause.Name() != nil {
				textRange := core.NewTextRange(importClause.Name().End(), namedBindings.End())
				return []rule.RuleFix{rule.RuleFixRemoveRange(textRange)}
			}
			return []rule.RuleFix{removeImportDeclaration(ctx, importDecl)}
		}

		return removeSpecifierFromList(ctx, decl, elements)

	case ast.IsImportClause(decl):
		importClause := decl.AsImportClause()
		if importClause.NamedBindings != nil {
			textRange := core.NewTextRange(decl.Name().Pos(), importClause.NamedBindings.Pos())
			return []rule.RuleFix{rule.RuleFixRemoveRange(textRange)}
		}
		return []rule.RuleFix{removeImportDeclaration(ctx, importDecl)}

	case ast.IsNamespaceImport(decl):
		importClause := decl.Parent
		if importClause != nil && importClause.Name() != nil {
			textRange := core.NewTextRange(importClause.Name().End(), decl.End())
			return []rule.RuleFix{rule.RuleFixRemoveRange(textRange)}
		}
		return []rule.RuleFix{removeImportDeclaration(ctx, importDecl)}
	}

	return nil
}

func removeSpecifierFromList(ctx rule.RuleContext, specifier *ast.Node, elements []*ast.Node) []rule.RuleFix {
	text := ctx.SourceFile.Text()
	for i, el := range elements {
		if el == specifier {
			if i == 0 && len(elements) > 1 {
				textRange := core.NewTextRange(el.Pos(), elements[i+1].Pos())
				return []rule.RuleFix{rule.RuleFixRemoveRange(textRange)}
			} else if i > 0 {
				// Remove just the specifier text itself (not the comma before it).
				// el.Pos() includes leading trivia (whitespace), so skip it to
				// find the actual specifier text start.
				start := el.Pos()
				for start < el.End() && (text[start] == ' ' || text[start] == '\t' || text[start] == '\n' || text[start] == '\r') {
					start++
				}
				textRange := core.NewTextRange(start, el.End())
				return []rule.RuleFix{rule.RuleFixRemoveRange(textRange)}
			}
		}
	}
	return nil
}

func removeImportDeclaration(ctx rule.RuleContext, importDecl *ast.Node) rule.RuleFix {
	text := ctx.SourceFile.Text()
	start := importDecl.Pos()
	end := importDecl.End()

	// Pos() includes leading trivia (newlines before the import statement).
	// Skip past leading newlines so we don't eat blank lines before the import.
	for start < end && (text[start] == '\n' || text[start] == '\r') {
		start++
	}
	// Walk backward to include any indentation at the start of this line.
	for start > 0 && text[start-1] != '\n' && text[start-1] != '\r' {
		start--
	}

	// Consume trailing whitespace and newline after the import statement.
	for end < len(text) && (text[end] == ' ' || text[end] == '\t') {
		end++
	}
	if end < len(text) && text[end] == '\n' {
		end++
	} else if end+1 < len(text) && text[end] == '\r' && text[end+1] == '\n' {
		end += 2
	}

	return rule.RuleFixRemoveRange(core.NewTextRange(start, end))
}

func findImportDeclaration(node *ast.Node) *ast.Node {
	current := node
	for current != nil {
		if ast.IsImportDeclaration(current) {
			return current
		}
		current = current.Parent
	}
	return nil
}

func getReportNode(symbol *ast.Symbol) *ast.Node {
	if len(symbol.Declarations) == 0 {
		return nil
	}
	decl := symbol.Declarations[0]
	name := decl.Name()
	if name != nil {
		return name
	}
	return decl
}

func getAction(symbol *ast.Symbol) string {
	for _, decl := range symbol.Declarations {
		if ast.IsVariableDeclaration(decl) || ast.IsBindingElement(decl) {
			return "assigned a value"
		}
	}
	return "defined"
}

func isExportedDeclaration(node *ast.Node) bool {
	if node == nil {
		return false
	}

	if ast.HasModifier(node, ast.ModifierFlagsExport) || ast.HasModifier(node, ast.ModifierFlagsExportDefault) {
		return true
	}

	parent := node.Parent
	if parent != nil {
		if parent.Kind == ast.KindVariableDeclarationList {
			parent = parent.Parent
		}
		if parent != nil && (ast.HasModifier(parent, ast.ModifierFlagsExport) || ast.HasModifier(parent, ast.ModifierFlagsExportDefault)) {
			return true
		}
	}

	return false
}

func isImportDeclaration(node *ast.Node) bool {
	return ast.IsImportClause(node) || ast.IsImportSpecifier(node) || ast.IsNamespaceImport(node)
}

func isParameter(node *ast.Node) bool {
	return ast.IsParameter(node)
}

func isCatchClauseVariable(node *ast.Node) bool {
	if !ast.IsVariableDeclaration(node) {
		return false
	}
	return node.Parent != nil && node.Parent.Kind == ast.KindCatchClause
}

func isArrayDestructure(node *ast.Node) bool {
	if ast.IsBindingElement(node) {
		return node.Parent != nil && ast.IsArrayBindingPattern(node.Parent)
	}
	return false
}

func isRestSibling(node *ast.Node) bool {
	if !ast.IsBindingElement(node) {
		return false
	}
	parent := node.Parent
	if parent == nil || !ast.IsObjectBindingPattern(parent) {
		return false
	}
	elements := parent.Elements()
	if len(elements) == 0 {
		return false
	}
	last := elements[len(elements)-1]
	if last == node {
		return false
	}
	return last.AsBindingElement().DotDotDotToken != nil
}

func isForInOfVariable(node *ast.Node) bool {
	if !ast.IsVariableDeclaration(node) {
		return false
	}
	parent := node.Parent
	if parent == nil || !ast.IsVariableDeclarationList(parent) {
		return false
	}
	grandparent := parent.Parent
	return grandparent != nil && (grandparent.Kind == ast.KindForInStatement || grandparent.Kind == ast.KindForOfStatement)
}

func isUsingDeclaration(node *ast.Node) bool {
	if !ast.IsVariableDeclaration(node) {
		return false
	}
	parent := node.Parent
	if parent == nil || !ast.IsVariableDeclarationList(parent) {
		return false
	}
	return parent.Flags&ast.NodeFlagsUsing != 0
}

func classHasStaticBlock(node *ast.Node) bool {
	for _, member := range node.Members() {
		if member.Kind == ast.KindClassStaticBlockDeclaration {
			return true
		}
	}
	return false
}

func isParameterUsedAfter(refs referenceMap, paramDecl *ast.Node) bool {
	parent := paramDecl.Parent
	if parent == nil {
		return false
	}
	params := parent.Parameters()
	if params == nil {
		return false
	}

	foundSelf := false
	for _, p := range params {
		if p == paramDecl {
			foundSelf = true
			continue
		}
		if foundSelf {
			sym := p.Symbol()
			if sym != nil && refs[sym] != 0 {
				return true
			}
		}
	}
	return false
}

func getAdditionalForIgnorePattern(co compiledOptions, symbol *ast.Symbol) string {
	for _, decl := range symbol.Declarations {
		if isParameter(decl) && co.argsIgnorePattern != nil {
			return "Allowed unused args must match " + co.argsIgnorePattern.String()
		}
		if isCatchClauseVariable(decl) && co.caughtErrorsIgnorePattern != nil {
			return "Allowed unused caught errors must match " + co.caughtErrorsIgnorePattern.String()
		}
		if isArrayDestructure(decl) && co.destructuredArrayIgnorePattern != nil {
			return "Allowed unused elements of array destructuring must match " + co.destructuredArrayIgnorePattern.String()
		}
		if co.varsIgnorePattern != nil {
			return "Allowed unused vars must match " + co.varsIgnorePattern.String()
		}
	}
	return ""
}
