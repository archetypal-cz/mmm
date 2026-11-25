// CP852 <-> Unicode (generated)
export const CP852 = "ÇüéâäůćçłëŐőîŹÄĆÉĹĺôöĽľŚśÖÜŤťŁ×čáíóúĄąŽžĘę¬źČş«»░▒▓│┤ÁÂĚŞ╣║╗╝Żż┐└┴┬├─┼Ăă╚╔╩╦╠═╬¤đĐĎËďŇÍÎě┘┌█▄ŢŮ▀ÓßÔŃńňŠšŔÚŕŰýÝţ´\xad˝˛ˇ˘§÷¸°¨˙űŘř■\xa0";
const ENC = new Map([...CP852].map((ch, i) => [ch, 128 + i]));
export function decode(bytes) {
  let s = "";
  for (const b of bytes) s += b < 128 ? String.fromCharCode(b) : CP852[b - 128];
  return s;
}
export function encode(str) {
  const out = [];
  for (const ch of str) {
    const c = ch.codePointAt(0);
    out.push(c < 128 ? c : (ENC.get(ch) ?? 63));
  }
  return Uint8Array.from(out);
}
