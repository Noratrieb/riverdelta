import { lines } from "./error";

it("should extract lines correctly", () => {
  const input = "AAA\nmeow\n:3\n\n";

  const lineSpans = lines({ content: input });
  const lineContents = lineSpans.map(({ start, end }) =>
    input.slice(start, end),
  );

  expect(lineContents).toStrictEqual(["AAA", "meow", ":3", "", ""]);
});
