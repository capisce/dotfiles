# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
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
  # networking.wireless.enable = true;
  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "en-latin9";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    coreutils
    curl
    dmenu
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-extras
    haskellPackages.xmonad-contrib
    kdeApplications.konsole
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

    # displayManager.sessionCommands = with pkgs; lib.mkAfter
    #   ''
    #   xmodmap /path/to/.Xmodmap
    #   '';

    windowManager.default = "xmonad";

    desktopManager.default = "none";
    desktopManager.xterm.enable = false;

    xkbVariant = "colemak";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
