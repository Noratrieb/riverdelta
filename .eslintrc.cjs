/* eslint-env node */
module.exports = {
  ignorePatterns: ["/target/**", "/jest.config.js"],
  extends: ["eslint:recommended", "plugin:@typescript-eslint/recommended"],
  parser: "@typescript-eslint/parser",
  plugins: ["@typescript-eslint"],
  root: true,
  rules: {
    // Sometimes you just need a `while(true)`.
    "no-constant-condition": "off",
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
  },
};
