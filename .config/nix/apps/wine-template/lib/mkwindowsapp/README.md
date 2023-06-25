# mkWindowsApp

`mkWindowsApp` is a Nix function which installs Wine-compatible Windows applications on [NixOS](https://nixos.org). `mkWindowsApp` retains some of the benefits provided by NixOS in the way Windows applications are installed. Namely:

 - Multiple versions of a Windows application can be installed simultaneously; Although only one can be active in the environment, of course.
 - Windows applications can be _rolled back_ along with other native Linux applications installed with Nix.
 - Installations are reproducible; Though sometimes not to the same extent as native applications.

`mkWindowsApp` now also features improved support for Windows games through the `enableVulkan` and `enableHUD` attributes.

## How does it work?

`mkWindowsApp` dynamically creates $WINEPREFIX-es (aka. Wine Bottles) at runtime using an overlay filesystem. The overlay filesystem consists of three filesystem layers (listed from lowest to highest):

 1. The _Windows_ layer: This layer consists of an initialized Wine Bottle. It can be used by multiple Windows applications, but no applications (other than the defaults installed by Wine) are installed in this layer. During runtime this layer is mounted read-only.
 2. The _App_ layer: This layer consists of the installed Windows application. During runtime this layer is mounted read-only.
 3. The _Read/Write_ layer: This layer can be written to and serves as the upper layer of the overlay. This layer is created at runtime and is discarded when the Windows application terminates.

These layers are stored not in the Nix store, but rather in `$HOME/.cache/mkWindows`. Hence, what actually happens is `mkWindows` creates a script[^1] which when executed installs and then runs a Windows application. The said script will create the necessary layers and then use them to create a Wine Bottle for the application to run in. Once the application terminates, the entire Wine Bottle is discarded, but the layers are left intact.

## FAQ

### Since Wine Bottles are temporary, what happens to files created when a Windows application is running? 
 
-Any files not saved outside of the Wine Bottle are discarded as well. Therefore, package maintainers must account for where important files are stored, such as configuration files, to ensure they are stored outside of the Wine Bottle; See the section [How to persist settings](#how-to-persist-settings). In addition, users must ensure to use the Z:\ drive when saving important information. 
 
### Do the layers need to be garbage-collected?

Yes, the package `mkwindowsapp-tools` has a garbage collector which should be run after `nix-collect-garbage`. You can run it as follows: `nix run github:emmanuelrosa/erosanix#mkwindows-tools`

### How does the garbage collector know what can be deleted? 

When the launcher script runs, it appends the path to the hidden copy of itself into the _references file_ of the windows and app layers. These _references files_ act like the Nix store roots; They _point_ to content, in this case, layers. The garbage collector simply checks if the paths in the _references files_ still exist in the Nix store. If they do, which indicates that the launcher script still exists, then the garbage collector knows that particular layer must not be deleted.

### How does an app know which layers to use?

Each layer is identified by an input hash, which similar to Nix, encapsulates the dependencies of the layer. The Windows layer input hash pre-image is the Nix store path to Wine and the internal `mkWindowsApp` API number. The app layer input hash pre-image is simply the script generated by `mkWindowsApp`, which encapsulates all of the inputs related to the application being installed. 

 ### How do you install a Windows application that uses `mkWindowsApp`?
 
 The same way you install any other Nix package. But note that installing the Nix package doesn't actually install the Windows application. The Windows application is installed when you run the (launcher) script which is installed by the Nix package. When a Windows application is installed non-interactively, it somewhat gives the illusion of having installed the application before-hand. However, there's a noticeable lag in startup time if layers need to be created.
 
### As an end-user, how will this work in practice? 

Let's use Notepad++ as an example: 

 1. First you need to have a NixOS system built as a Nix Flake, since this repo is a Nix Flake. 
 2. Next, you add the Nix Flake `github:emmanuelrosa/erosanix` as an input; I recommend setting it up to follow your Nixpkgs input. 
 3. Then, you add the package `erosanix.packages."${system}".notepad-plus-plus` to your `environment.systemPackages`. 
 4. Next, run `nixos-rebuilt switch`.
 5. Use your favorite app launcher UI (or the command line) to run `notepad++`. Alternatively, you can use `xdg-open` or a file manager to open a text file.
 6. If a _Windows layer_ needs to be created, you'll see a notification from Wine. Then, Notepad++ will be installed and launched.
 
### How can I package a Windows application with `mkWindowsApp`?

I recommend studying the example [sumatrapdf-nix](https://github.com/emmanuelrosa/sumatrapdf-nix). It's a Nix Flake which uses `mkWindowsApp` to package SumatraPDF.

## How to persist settings

Early releases of `mkWindowsApp` required package maintainers to handle the persistance of files which need to be retained across multiple executions. Usually, these are configuration files, or the entire user registry (user.reg). However, newer releases of `mkWindowsApp` provide the attribute `fileMap` which lets package maintainers easily set up what files to link into the $WINEPREFIX. Here's an example of how to use the attribute:

```
mkWindowsApp rec {
  ...
  fileMap = { "$HOME/.config/Notepad++" = "drive_c/users/$USER/Application Data/Notepad++"; 
  };
```

Using the example above, here's an explanation of what will happen when users run the launcher script:

 1. Before running Notepad++, `mkWindowsApp` will look for $HOME/.config/Notepad++, and if it exists, it will create a symlink to it at $WINEPREFIX/drive_c/users/$USER/Application Data/Notepad++. Alternatively, if $WINEPREFIX/drive_c/users/$USER/Application Data/Notepad++ exists but $HOME/.config/Notepad++ doesn't, then the file/directory will be copied out from the $WINEPREFIX into the "source" path, and then symlinked as described earlier.
 2. After Notepad++ terminates, `mkWindowsApp` will cycle through the same list of mappings and copy any of the files/directories which did not exist when the application was launched, to the "source" path. This effectively persists such files so that they can be symlinked the next time the application is launched. 

By default, the Wine registry is not persisted. To enable automatic persistence of the Wine registry files, set the `persistRegistry` attribute in your package to `true`. The registry files are saved at `$HOME/.config/mkWindowsApp/${pname}`.

For a real-world example see [sumatrapdf-nix](https://github.com/emmanuelrosa/sumatrapdf-nix).

## How can I access Wine tools such as winecfg?

NOTICE: By default, the Wine registry is not persisted, so if you want to use winecfg to tweak things you need to enable registry persistence. See the section *How to persist settings*.

There's now an environment variable which can be used to get dropped into a shell after setting up the WINEPREFIX. Simply set the environment variable `WA_RUN_APP=0` before running the app (launcher). When `WA_RUN_APP` is not set to `1`, the WINEPREFIX is set up, but the app is not executed. Once in the shell, you can run Wine tools; The WINEPREFIX will already be set.

[^1]: The script is based on [wrapWine](https://github.com/lucasew/nixcfg/blob/fd523e15ccd7ec2fd86a3c9bc4611b78f4e51608/packages/wrapWine.nix).