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
    wget
    vim
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services.nixosManual.showManual = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.twoFingerScroll = true;

  # services.xserver = {
  #   windowManager.xmonad = {
  #     enable = true;
  #     enableContribAndExtras = true;
  #     # extraPackages = haskellPackages: [
  #     #   haskelPackages.xmonad-contrib
  #     #   haskelPackages.xmonad-extras
  #     #   haskelPackages.xmonad
  #     # ];
  #   };
  #   desktopManager.default = "gnome3";
  #   desktopManager.gnome3.enable = true;
  #   # windowManager.default = "xmonad";
  # };

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
