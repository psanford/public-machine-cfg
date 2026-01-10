{ lib
, stdenv
, fetchurl
, autoPatchelfHook
}:

let
  version = "0.0.1-rc29";

  sources = {
    x86_64-linux = {
      url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-linux-amd64.tar.gz";
      hash = "sha256-4AVjv/ZWM4QYmOJFz9/ky1w3cO9Vc93Tq0Voa2dRfP4=";
    };
    aarch64-linux = {
      url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-linux-arm64.tar.gz";
      hash = "sha256-PECd9HjRFNLMfVBEvNZhyU1JsRdwOY2hmaOdyq4Qd9Y=";
    };
    x86_64-darwin = {
      url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-darwin-amd64.tar.gz";
      hash = "sha256-dJLI0Cn+8MhOtf7Id6/Pnlxz1+GYYhTsQI56TgBmE6U=";
    };
    aarch64-darwin = {
      url = "https://sprites-binaries.t3.storage.dev/client/v${version}/sprite-darwin-arm64.tar.gz";
      hash = "sha256-HyEHGQF0S9BaKp80jZ8NzcwgtUFGrmycs5cJbCJJTs8=";
    };
  };

  src = fetchurl sources.${stdenv.hostPlatform.system};

in stdenv.mkDerivation {
  pname = "sprite-cli";
  inherit version src;

  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    install -Dm755 sprite $out/bin/sprite
    runHook postInstall
  '';

  meta = {
    description = "CLI tool for sprites.dev web hosting service";
    homepage = "https://sprites.dev";
    license = lib.licenses.unfree;
    platforms = builtins.attrNames sources;
    mainProgram = "sprite";
  };
}

# To update this package:
# 1. Get the latest version (try release.txt first, fall back to rc.txt):
#    curl -sS https://sprites-binaries.t3.storage.dev/client/release.txt
#    curl -sS https://sprites-binaries.t3.storage.dev/client/rc.txt
#
# 2. Update the version below (without the 'v' prefix)
#
# 3. Get new hashes for each platform:
#    nix-prefetch-url https://sprites-binaries.t3.storage.dev/client/v<VERSION>/sprite-linux-amd64.tar.gz
#    nix-prefetch-url https://sprites-binaries.t3.storage.dev/client/v<VERSION>/sprite-linux-arm64.tar.gz
#    nix-prefetch-url https://sprites-binaries.t3.storage.dev/client/v<VERSION>/sprite-darwin-amd64.tar.gz
#    nix-prefetch-url https://sprites-binaries.t3.storage.dev/client/v<VERSION>/sprite-darwin-arm64.tar.gz
#
# 4. Convert each hash to SRI format:
#    nix hash convert --hash-algo sha256 <hash>
#
# 5. Update the hash values below
