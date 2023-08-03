# riverdelta

language support for the [riverdelta](https://github.com/Nilstrieb/riverdelta) language.

# installation

## normal

copy this directory into your `.vscode/extensions`

## nix

if you use nix to manage extensions you can just copy

```nix
pkgs.vscode-utils.buildVscodeExtension {
    name = "riverdelta";
    version = "0.1.0";
    src = builtins.fetchGit {
        url = "https://github.com/Nilstrieb/riverdelta";
        rev = "86dec70f686964615a93c5316aae460bc0fc5d6d";
    };
    vscodeExtPublisher = "Nilstrieb";
    vscodeExtName = "riverdelta";
    vscodeExtUniqueId = "Nilstrieb.riverdelta";
    buildPhase = ''
        runHook preBuild;
        cd ./vscode
        runHook postBuild;
    '';
}
```

which probably works i think (maybe you want to update the hash to something newer although its unlikey this extension will change that much)
