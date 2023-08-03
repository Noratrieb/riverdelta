/* eslint-env node */
module.exports = {
  ignorePatterns: ["/target/**", "/*.js", "/*.cjs"],
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/strict-type-checked",
    "plugin:@typescript-eslint/stylistic-type-checked",
  ],
  parser: "@typescript-eslint/parser",
  parserOptions: {
    project: true,
    tsconfigRootDir: __dirname,
  },
  plugins: ["@typescript-eslint"],
  root: true,
  rules: {
    // Some silly rules forbidding things that are not wrong:
    "no-constant-condition": "off",
    "no-empty": "off",
    "@typescript-eslint/no-empty-function": "off",
    // Typescript already checks problematic fallthrough.
    // The eslint rule is a bit dumb and also complains about
    // obvious clear fallthrough like `case "a": case "b"`.
    "no-fallthrough": "off",
    // Suppress no-unused-vars with leading underscores.
    "@typescript-eslint/no-unused-vars": [
      "warn",
      {
        varsIgnorePattern: "^_",
        argsIgnorePattern: "^_",
      },
    ],
    // `any` is genrally bad, but sometimes it's the nicest solution
    // Just let me use it without any ceremony.
    "@typescript-eslint/no-explicit-any": "off",
    "@typescript-eslint/no-unsafe-member-access": "off",
    // This needs to be turned off until the AST types are redesigned.
    "@typescript-eslint/no-non-null-assertion": "off",
    // "value is always truthy" YES IT IS. Typescript already emits errors
    // for the important cases here.
    "@typescript-eslint/no-unnecessary-condition": "off",
    "@typescript-eslint/no-confusing-void-expression": [
      "error",
      {
        ignoreArrowShorthand: true,
      },
    ],
    // No, I will use `type` instead of `interface`.
    "@typescript-eslint/consistent-type-definitions": ["error", "type"],

    // This lint is horrible with noisy false positives every time there are typescript errors.
    "@typescript-eslint/no-unsafe-return": "off",
    "@typescript-eslint/no-unsafe-assignment": "off",
    "@typescript-eslint/no-unsafe-argument": "off",

    // Useful extra lints that are not on by default:
    "@typescript-eslint/explicit-module-boundary-types": "warn",
    // This has caused several bugs before. Thanks eslint!
    "@typescript-eslint/strict-boolean-expressions": [
      "error",
      {
        allowNullableObject: true,
        allowNullableBoolean: false,
      },
    ],
  },
};
