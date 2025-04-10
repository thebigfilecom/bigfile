{ config, lib, pkgs, ... }:

let
  cfg = config.services.bigfile;
  bigfilePkg = (pkgs.callPackage ./bigfile.nix {
    inherit pkgs;
    crashDumpsDir = cfg.crashDumpsDir;
    erlangCookie = cfg.erlangCookie;
    vcPatches = cfg.patches;
  }).bigfile;
  generatedConfigFile = "${import ./generate-config.nix { bigfileConfig = cfg; inherit pkgs; }}";
  bigfile-service-start =
    let
      command = "${cfg.package}/bin/start-nix-foreground config_file ${cfg.configFile}";
      peers = "${builtins.concatStringsSep " " (builtins.concatMap (p: ["peer" p]) cfg.peer)}";
      vdf-peers = "${builtins.concatStringsSep " " (builtins.concatMap (p: ["vdf_client_peer" p]) cfg.vdfClientPeer)}";
      vdf-server-peers = "${builtins.concatStringsSep " " (builtins.concatMap (p: ["vdf_server_trusted_peer" p]) cfg.vdfServerTrustedPeer)}";
    in
    pkgs.writeScriptBin "bigfile-start" ''
      #!${pkgs.bash}/bin/bash
      # Function to handle termination and cleanup
      cleanup() {
        echo "Terminating erl and killing epmd..."
        kill $(${pkgs.procps}/bin/pgrep epmd) || true
        exit 0
      }

      # Set up a trap to call the cleanup function when the script is terminated
      trap cleanup INT TERM
      ${command} ${peers} ${vdf-peers} ${vdf-server-peers} &
      BIGFILE_ERL_PID=$! # capture PID of the background process
      i=0
      until [[ "$(${pkgs.procps}/bin/ps -C beam &> /dev/null)" -eq 0 || "$i" -ge "200" ]]
      do
        sleep 1
        i=$((i+1))
      done
      if [[ "$i" -ge "200" ]]; then
        echo "beam process failed to start"
        exit 0
      fi
      echo "beam process started..."
      wait $BIGFILE_ERL_PID || true
      counter=0
      until [[ "$(ps -C beam &> /dev/null)" -ne 0 ]] || [[ $counter -ge 30 ]]
      do
        sleep 1
        let counter++
      done
      cleanup
    '';

in
{

  options.services.bigfile = import ./options.nix {
    inherit lib;
    defaultBigfileConfigFile = generatedConfigFile;
    defaultBigfilePackage = bigfilePkg;
  };

  config = lib.mkIf cfg.enable {
    systemd.services.bigfile-screen = {
      enable = false;
    };

    systemd.services.bigfile = {
      after = [ "network.target" ];
      serviceConfig.Type = "simple";
      serviceConfig.ExecStart = "${bigfile-service-start}/bin/bigfile-start";
      serviceConfig.TimeoutStartSec = "60";
      serviceConfig.ExecStop = "${pkgs.bash}/bin/bash -c '${cfg.package}/bin/stop-nix || true; ${pkgs.procps}/bin/pkill beam || true; sleep 15'";
      serviceConfig.TimeoutStopSec = "120";
      serviceConfig.RestartKillSignal = "SIGINT";
    };
  };
}
