export function encodeUtf8(s: string): Uint8Array {
    return new TextEncoder().encode(s);
}