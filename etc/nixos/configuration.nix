{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  boot.loader = {
    grub.device = "/dev/sda";
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda3";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  networking.hostName = "nixus";
  networking.networkmanager.enable = true;

  nix = {
    buildCores = 4;

    # http://anderspapitto.com/posts/2015-11-01-nixos-with-local-nixpkgs-checkout.html
    nixPath = pkgs.lib.mkBefore [ "/etc/nixos" "nixos-config=/etc/nixos/configuration.nix" ];
  };

  i18n = {
    consoleFont = "ter-k24n";
    consoleKeyMap = "en-latin9";
    defaultLocale = "en_US.UTF-8";

    consolePackages = with pkgs; [ terminus_font ];

    inputMethod = {
      enabled = "fcitx";
      fcitx.engines = with pkgs.fcitx-engines; [ unikey ];
    };
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      inconsolata
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      source-code-pro
      symbola
    ];
  };

  environment.systemPackages = with pkgs; [
    coreutils
    curl
    dmenu
    ctags
    emacs
    ghc
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-extras
    haskellPackages.xmonad-contrib
    htop
    kdeApplications.konsole
    networkmanagerapplet
    psmisc
    stalonetray
    stow
    vim
    vimPlugins.pathogen
    wget
    xclip
    xorg.xmodmap
    xscreensaver
  ];

  hardware.pulseaudio.enable = true;

  programs.bash.enableCompletion = true;

  services.emacs = {
    enable = true;
    install = true;
    defaultEditor = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.nixosManual.showManual = true;

  services.mingetty = import ./mingetty.nix { inherit config; };

  services.xserver = {
    enable = true;
    layout = "us";

    # symlink X server configuration under /etc/X11/xorg.conf
    exportConfiguration = true;

    libinput = {
      enable = true;
      tapping = false;

      accelProfile = "flat";
      accelSpeed = "0.05";
      naturalScrolling = true;
      horizontalScrolling = false;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
        haskellPackages.xmonad
      ];
    };

    displayManager = {
      slim.enable = true;
      slim.defaultUser = "srodal";
      sessionCommands = with pkgs; lib.mkAfter
        ''
        xscreensaver -no-splash &
        nm-applet --sm-disable &
        '';

    };

    windowManager.default = "xmonad";

    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

    xkbVariant = "colemak";
  };

  services.redshift =
  {
    enable = true;
    temperature.night = 2200;
  } // import ./redshift.nix;

  time.timeZone = null;

  users.extraUsers.srodal = {
    name = "srodal";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
    ];
    isNormalUser = true;
    uid = 1000;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  # Auto-upgrade?
  # To see when the service runs, see systemctl list-timers
  # system.autoUpgrade.enable = true;
}
