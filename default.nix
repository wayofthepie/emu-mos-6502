{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "emu-mos6502";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/wayofthepie/emu-mos6502";
  description = "A MOS Technologies 6502 CPU emulator";
  license = stdenv.lib.licenses.mit;
}
