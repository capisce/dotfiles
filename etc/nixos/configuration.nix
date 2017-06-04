{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda3";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "nixus";
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "en-latin9";
    defaultLocale = "en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    coreutils
    curl
    dmenu
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-extras
    haskellPackages.xmonad-contrib
    htop
    kdeApplications.konsole
    psmisc
    vim
    wget
    xclip
    xorg.xmodmap
  ];

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  services.nixosManual.showManual = true;

  services.xserver = {
    enable = true;
    layout = "us";

    # symlink X server configuration under /etc/X11/xorg.conf
    exportConfiguration = true;

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      tapButtons = false;

      accelFactor = "0.015";
      minSpeed = "0.8";
      maxSpeed = "1.4";

      # reverse vertical scrolling (like on Mac OS X)
      additionalOptions = ''
        Option "VertScrollDelta" "-100"
        Option "HorizScrollDelta" "100"
      '';
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
    };

    windowManager.default = "xmonad";

    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

    xkbVariant = "colemak";
  };

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
  system.stateVersion = "17.03";
  
  # Auto-upgrade?
  # To see when the service runs, see systemctl list-timers
  # system.autoUpgrade.enable = true;
}
