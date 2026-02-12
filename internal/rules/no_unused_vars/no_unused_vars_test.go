package no_unused_vars

import (
	"testing"

	"github.com/typescript-eslint/tsgolint/internal/rule_tester"
	"github.com/typescript-eslint/tsgolint/internal/rules/fixtures"
)

func TestNoUnusedVars(t *testing.T) {
	t.Parallel()
	rule_tester.RunRuleTester(fixtures.GetRootDir(), "tsconfig.minimal.json", t, &NoUnusedVarsRule, validCases, invalidCases)
}

var validCases = []rule_tester.ValidTestCase{
	// Basic usage
	{Code: `const x = 1; console.log(x);`},
	{Code: `function foo() { return 1; } foo();`},
	{Code: `class Foo {} new Foo();`},
	{Code: `interface Foo {} const x: Foo = {}; console.log(x);`},
	{Code: `type Foo = string; const x: Foo = 'hello'; console.log(x);`},
	{Code: `enum Foo { A } console.log(Foo.A);`},

	// Exports are always used
	{Code: `export const x = 1;`},
	{Code: `export function foo() {}`},
	{Code: `export class Foo {}`},
	{Code: `export interface Foo {}`},
	{Code: `export type Foo = string;`},
	{Code: `export enum Foo { A }`},
	{Code: `export default function foo() {}`},

	// Type imports used as types
	{Code: `
import { Nullable } from 'nullable';
const a: Nullable = null;
console.log(a);
	`},
	{Code: `
import { Nullable } from 'nullable';
function foo(a: Nullable) { console.log(a); }
foo();
	`},
	{Code: `
import { Nullable } from 'nullable';
class A { do(): Nullable { return null; } }
new A();
	`},

	// Import used in extends/implements
	{Code: `
import { Nullable } from 'nullable';
class A extends Nullable {}
new A();
	`},
	{Code: `
import { Nullable } from 'nullable';
export interface A extends Nullable {}
	`},

	// Decorators
	{Code: `
import { ClassDecoratorFactory } from 'decorators';
@ClassDecoratorFactory()
export class Foo {}
	`},
	{Code: `
import { Foo, Bar } from 'decorators';
@Foo(Bar)
export class Baz {}
	`},

	// Namespaces with merged declarations
	{Code: `
function Foo() {}
namespace Foo {
  export function Bar() {}
}
Foo.Bar();
	`},

	// Enum used as both type and value
	{Code: `
enum Direction { Up, Down }
function move(d: Direction) { console.log(d); }
move(Direction.Up);
	`},

	// Variables in for-in/for-of with _ prefix
	{Code: `for (const _x in {}) {}`},
	{Code: `for (const _x of []) {}`},

	// Args: "none" - all args are ignored
	{
		Code:    `function foo(x: number) { return; } foo(1);`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"args": "none"}`),
	},

	// Args: "after-used" - unused args before used ones are ok
	{Code: `function foo(_a: number, b: number) { return b; } foo(1, 2);`},

	// varsIgnorePattern
	{
		Code:    `const _unused = 1;`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"varsIgnorePattern": "^_"}`),
	},

	// argsIgnorePattern
	{
		Code:    `function foo(_x: number) {} foo(1);`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"argsIgnorePattern": "^_"}`),
	},

	// caughtErrors: "none"
	{
		Code:    `try {} catch (e) {}`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"caughtErrors": "none"}`),
	},

	// caughtErrorsIgnorePattern
	{
		Code:    `try {} catch (_e) {}`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"caughtErrorsIgnorePattern": "^_"}`),
	},

	// ignoreRestSiblings
	{
		Code:    `const { a, ...rest } = { a: 1, b: 2, c: 3 }; console.log(rest);`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"ignoreRestSiblings": true}`),
	},

	// destructuredArrayIgnorePattern
	{
		Code:    `const [_a, b] = [1, 2]; console.log(b);`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"destructuredArrayIgnorePattern": "^_"}`),
	},

	// vars: "local" - top-level vars are not checked
	{
		Code:    `const unused = 1;`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"vars": "local"}`),
	},

	// Classes with static init blocks
	{
		Code: `
class Foo {
  static {
    console.log('initialized');
  }
}
		`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"ignoreClassWithStaticInitBlock": true}`),
	},

	// TypeScript-specific: type used in generic
	{Code: `
import { Nullable } from 'nullable';
function foo<T extends Nullable>(x: T) { return x; }
foo(null);
	`},

	// Self-referencing is fine if used elsewhere
	{Code: `
function foo() { return foo; }
console.log(foo());
	`},

	// Variable used in typeof
	{Code: `
const x = 1;
type T = typeof x;
const y: T = 2;
console.log(y);
	`},

	// Import used only as type parameter
	{Code: `
import { Nullable } from 'nullable';
export type Foo = Nullable;
	`},

	// Namespace self-reference
	{Code: `
export namespace Foo {
  export function bar() {}
  export function baz() { bar(); }
}
	`},

	// Class used as type
	{Code: `
class Foo {}
function bar(x: Foo) { console.log(x); }
bar(new Foo());
	`},
}

var invalidCases = []rule_tester.InvalidTestCase{
	// Basic unused variable
	{
		Code:   `const x = 1;`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},
	// Basic unused function
	{
		Code:   `function foo() {}`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},
	// Basic unused class
	{
		Code:   `class Foo {}`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},
	// Basic unused interface
	{
		Code:   `interface Foo {}`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},
	// Basic unused type alias
	{
		Code:   `type Foo = string;`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},
	// Basic unused enum
	{
		Code:   `enum Foo { A }`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// Unused import
	{
		Code: `
import { ClassDecoratorFactory } from 'decorators';
export class Foo {}
		`,
		Errors: []rule_tester.InvalidTestCaseError{
			{
				MessageId: "unusedVar",
				Line:      2,
				Suggestions: []rule_tester.InvalidTestCaseSuggestion{
					{
						MessageId: "removeUnusedImportDeclaration",
						Output: `
export class Foo {}
		`,
					},
				},
			},
		},
	},

	// One unused import of multiple
	{
		Code: `
import { Nullable } from 'nullable';
import { Another } from 'some';
class A {
  do(a: Nullable) {
    console.log(a);
  }
}
new A();
		`,
		Errors: []rule_tester.InvalidTestCaseError{
			{
				MessageId: "unusedVar",
				Line:      3,
				Suggestions: []rule_tester.InvalidTestCaseSuggestion{
					{
						MessageId: "removeUnusedImportDeclaration",
						Output: `
import { Nullable } from 'nullable';
class A {
  do(a: Nullable) {
    console.log(a);
  }
}
new A();
		`,
					},
				},
			},
		},
	},

	// Unused parameter - args: "all"
	{
		Code:    `function foo(x: number) {} foo(1);`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"args": "all"}`),
		Errors:  []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// Unused parameter - args: "after-used"
	{
		Code:   `function foo(a: number, b: number) { return a; } foo(1, 2);`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// Caught error unused
	{
		Code:   `try {} catch (e) {}`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// Unused variable with varsIgnorePattern not matching
	{
		Code:    `const x = 1;`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"varsIgnorePattern": "^_"}`),
		Errors:  []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// reportUsedIgnorePattern - variable matches pattern but is used
	{
		Code:    `const _x = 1; console.log(_x);`,
		Options: rule_tester.OptionsFromJSON[NoUnusedVarsOptions](`{"varsIgnorePattern": "^_", "reportUsedIgnorePattern": true}`),
		Errors:  []rule_tester.InvalidTestCaseError{{MessageId: "usedIgnoredVar"}},
	},

	// Multiple unused variables
	{
		Code:   `const a = 1; const b = 2;`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}, {MessageId: "unusedVar"}},
	},

	// Destructured array - unused element
	{
		Code:   `const [a] = [1];`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// Import used only as type with value binding
	// (no usedOnlyAsType - imports used as types are fine in TS)

	// Unused in nested scope
	{
		Code: `
function foo() {
  const x = 1;
  return;
}
foo();
		`,
		Errors: []rule_tester.InvalidTestCaseError{{MessageId: "unusedVar"}},
	},

	// Multiple imports, one unused
	{
		Code: `
import { Foo, Bar } from 'foo';
console.log(Foo);
		`,
		Errors: []rule_tester.InvalidTestCaseError{
			{
				MessageId: "unusedVar",
				Suggestions: []rule_tester.InvalidTestCaseSuggestion{
					{
						MessageId: "removeUnusedImportDeclaration",
						Output: `
import { Foo,  } from 'foo';
console.log(Foo);
		`,
					},
				},
			},
		},
	},
}
