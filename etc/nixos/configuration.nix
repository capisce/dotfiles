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

  boot.kernelPackages = pkgs.linuxPackages_latest;

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  networking.hostName = "nixus";
  networking.networkmanager.enable = true;

  nix = {
    buildCores = 4;

    # http://anderspapitto.com/posts/2015-11-01-nixos-with-local-nixpkgs-checkout.html
    nixPath = [
      "nixpkgs=/etc/nixos/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
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
    ag
    coreutils
    curl
    dmenu
    ctags
    emacs
    firefox
    ghc
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-extras
    haskellPackages.xmonad-contrib
    htop
    kdeApplications.konsole
    networkmanagerapplet
    nodePackages.tern
    psmisc
    reptyr # process reparenting
    scrot
    stalonetray
    stow
    tmux
    vim
    vimPlugins.pathogen
    wget
    xclip
    xorg.xmodmap
    xscreensaver
  ];

  hardware.pulseaudio.enable = true;

  programs.bash.enableCompletion = true;
  programs.chrome-token-signing.enable = true;

  services.emacs = {
    enable = true;
    install = true;
    defaultEditor = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.nixosManual.showManual = true;

  services.mingetty = import ./mingetty.nix { inherit config; };

  services.pcscd.enable = true;

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
    temperature.day = 8000;
    temperature.night = 3000;
    brightness.night = "0.7";
  } // import ./redshift.nix;

  time.timeZone = "Europe/Tallinn";

  users.extraUsers.srodal = {
    name = "srodal";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
    ];
    isNormalUser = true;
    uid = 1000;
  };

  zramSwap = {
    enable = true;
    memoryPercent = 20;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";

  # Auto-upgrade?
  # To see when the service runs, see systemctl list-timers
  # system.autoUpgrade.enable = true;
}
