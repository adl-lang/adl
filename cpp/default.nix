with import <nixpkgs> {}; {
  adlEnv = stdenv.mkDerivation {
    name = "adl";
    buildInputs = [stdenv pkgconfig automake autoconf boost cpp-netlib asio];
  };
}