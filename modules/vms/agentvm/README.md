# agentvm

A headless NixOS VM that runs the orchestrator daemon.
The daemon enrols with an existing orchestrator server
and runs Claude Code agents for the server's tasks.

The VM is set up as the server sets up the VPSes it provisions:

- the daemon runs as `orchestrator`, with its state in `/var/lib/orchestrator`;
- Claude Code and every workspace command run as the harness user `orch-agent`
  through sudo, in `/srv/orchestrator/workspaces`;
- the daemon only dials out to the server, so the VM opens only ssh inbound,
  for administration.

You upload the daemon binary to `/usr/local/bin/orchestrator-daemon`
after the first boot.

## Prerequisites

- An orchestrator server running with `-ca-key`,
  and the address the daemon reaches it at (its `-public-url`).
  The server's certificate must name that host.
- The server's CA certificate, `ca.crt`.
- A daemon binary for linux-amd64,
  built with `scripts/build-release.sh` in the orchestrator repository.

## Deployment inputs

The VM reads the server URL, CA and enrolment token at boot
as [systemd credentials][] passed by the hypervisor,
so the enrolment token stays out of the world-readable Nix store.

| Credential                 | Value                                       | Needed      |
| -------------------------- | ------------------------------------------- | ----------- |
| `orchestrator.server-url`  | The server's `https://` daemon address      | Every boot  |
| `orchestrator.ca`          | The server's CA certificate (PEM)           | Every boot  |
| `orchestrator.enrol-token` | `<daemon-id>:<secret>`                      | First start |

## Setup

1. Build the VM:

   ```sh
   nix build .#nixosConfigurations.agentvm.config.system.build.vm
   ```

2. On the server, create an enrolment token.
   The token's id becomes the daemon's id.
   It is single use and expires after an hour by default,
   so finish the remaining steps within that time,
   or pass a longer `-lifetime`.

   ```sh
   orchestrator-server enrol-token -db <db> -id <daemon-id>
   ```

3. Boot the VM with the credentials.
   The disk image is created as `./agentvm.qcow2` in the current directory
   (override with `NIX_DISK_IMAGE`).
   The VM's serial console takes over the terminal.

   ```sh
   QEMU_OPTS="\
   -smbios type=11,value=io.systemd.credential:orchestrator.server-url=https://SERVER:PORT \
   -smbios type=11,value=io.systemd.credential:orchestrator.enrol-token=DAEMON_ID:SECRET \
   -smbios type=11,value=io.systemd.credential.binary:orchestrator.ca=$(base64 -w0 ca.crt)" \
     ./result/bin/run-agentvm-vm
   ```

4. From another terminal, upload and install the daemon binary.
   The VM forwards `127.0.0.1:2222` on the host to its ssh port.
   Log in as `admin` with the password `agentvm`.

   ```sh
   scp -P 2222 daemon-<version>-linux-amd64 admin@127.0.0.1:
   ssh -t -p 2222 admin@127.0.0.1 \
     sudo install -D -m 0755 daemon-<version>-linux-amd64 /usr/local/bin/orchestrator-daemon
   ```

   The service retries every 10 seconds until the binary is present,
   then enrols with the server.
   Follow it with:

   ```sh
   ssh -t -p 2222 admin@127.0.0.1 journalctl -fu orchestrator-daemon
   ```

5. In the server's GUI, on Infrastructure, open the daemon's page and:
   1. Log Claude Code in: open the link it shows, authorise,
      and paste the code back.
      The server assigns work to the daemon only after this login.
   2. Set the `models` label, for example `models=sonnet;haiku`.
   3. Register the push key it shows at the forge,
      as a deploy key with write access or on a machine user.
   4. For forges other than GitHub, add their host keys on `/settings`.

Later boots need `orchestrator.server-url` and `orchestrator.ca` only.
The daemon reads its certificate from `/var/lib/orchestrator`
and ignores the token.

## Verification

- The daemon log has a "running tasks as the harness user" line
  naming the git and rm paths,
  and a "login" line reporting Claude Code's login state and account.
- `ps` shows `claude` running as `orch-agent` with a `sudo` parent.
- As `orch-agent`, `ls /var/lib/orchestrator` is denied.
- A task with a repository pushes its branch.

## Running the server on the host

QEMU's user networking forwards the guest's `10.0.2.2` to the host's loopback,
so a daemon listener bound to `127.0.0.1` on the host is reachable from the VM.

- Issue the server's certificate for that address:

  ```sh
  orchestrator-server issue-server-cert -pki-dir DIR -host 10.0.2.2
  ```

- Pass `https://10.0.2.2:PORT` as `orchestrator.server-url`.

The daemon accepts plain http only for a loopback address such as `127.0.0.1`.
Inside the VM `10.0.2.2` is not a loopback address,
so a server started with `-insecure-loopback` is not usable here.

## Notes

- **State lives in the disk image.**
  `/var/lib/orchestrator` holds the daemon's certificate and push key,
  and `~orch-agent/.claude` holds Claude Code's login.
  Deleting `agentvm.qcow2` means enrolling again
  with a new token and registering a new push key.
- **The disk size applies only to a new image.**
  Changing `virtualisation.vmVariant.virtualisation.diskSize` in `default.nix`
  has no effect on an existing `agentvm.qcow2`.
- **The token is visible in process lists.**
  It is on the host's qemu command line,
  and on the daemon's command line inside the VM,
  because the daemon accepts it only as a flag.
  It is single use and expires after its `-lifetime`, an hour by default.
- **Agent tools come from the service's `PATH`.**
  Claude Code inherits the daemon's `PATH`,
  so add agents' tools to `systemd.services.orchestrator-daemon.path` in `default.nix`.
  Packages in `environment.systemPackages` are not on the agents' `PATH`.
  git and coreutils must stay first:
  the daemon runs git and rm by the paths it finds there,
  and the sudoers rule names the same store paths.
  For language toolchains, agents can use `nix develop` with a project's flake.
- **Change the `admin` password** after the first login.

[systemd credentials]: https://systemd.io/CREDENTIALS/
