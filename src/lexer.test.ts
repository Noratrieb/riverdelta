import { tokenize } from "./lexer";

it('should tokenize an emtpy function', () => {
    const input = `function hello() {}`;

    const tokens = tokenize(input);

    expect(tokens).toMatchSnapshot();
});

it('should tokenize hello world', () => {
    const input = `print("hello world")`;

    const tokens = tokenize(input);

    expect(tokens).toMatchSnapshot();
});