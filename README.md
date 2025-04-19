# BigFile Server

This is the repository for the official Erlang implementation of the Bigfile
protocol and a gateway implementation.

BigFile is an AI-powered decentralized blockchain network engineered to provide decentralized, immutable, and scalable data storage solutions. It is designed to securely host applications, files, and Big Data, while facilitating computations on smart contracts at unparalleled scalability.

"The Bigfile source code was developed by modifying and enhancing the original Arweave project's source code, preserving its core functionality. However, the Bigfile project operates independently and has no affiliation with Arweave; it is considered an entirely separate blockchain network and project." [arweave](https://github.com/ArweaveTeam/arweave)

# Getting Started

Download and extract the latest archive for your platform on the release
page, then run the included `bin/start` script to get started.

For more information, refer to the [mining guide]().

# Building from source

## Requirements

The full bigfile node functionality is only supported on Linux, but it is possible to run a
VDF Server on MacOS. Refer to the [mining VDF guide]()
for more information on running your own VDF server.

**General requirements:**
- OpenSSL 1.1.1+
- OpenSSL development headers
- GCC or Clang (GCC 8+ recommended)
- Erlang OTP v24, with OpenSSL support
- GNU Make
- CMake (CMake version > 3.10.0)
- SQLite3 header
- GNU MP

<details>
  <summary>To install the dependencies on <b>Ubuntu 22 (recommended)</b>:</summary>
  </br>

  ```sh
  sudo apt install erlang libssl-dev libgmp-dev libsqlite3-dev make cmake gcc g++
  ```

  On some systems you might need to install `libncurses-dev`.
</details>
</br>
<details>
  <summary>To install the dependencies on <b>MacOS</b>:</summary>  
  </br>

  1. Install [Homebrew](https://brew.sh/)
  2. Install dependencies
  ```sh
  brew install gmp openssl@1.1 erlang@24 cmake pkg-config
  ```
  3. Homebrew may ask you to update your `LDFLAGS` for erlang: don't. You should however
  update your `PATH` as requested.

  **Notes:** 
  1. This process has only been tested on a fresh install of MacOS Ventura running on a Mac Mini M2. It may or may not work on other configurations.
  2. We have not validated mining or packing on MacOS, but as of May, 2024 the M2 provides the fastest known VDF implementation and so makes a good candidate for [VDF Servers]().

</details>
</br>


**Download the repo:**

```sh
$ git clone --recursive https://github.com/thebigfilecom/bigfile.git
$ cd bigfile
```

Increase the [open file
limits]().

**Run in the development mode:**

```sh
./bigfile-server \
peer testnet.thebigfile.info
```

**Make a production build:**

```sh
$ ./rebar3 as prod tar
```

You will then find the gzipped tarball at `_build/prod/rel/bigfile/bigfile-x.y.z.tar.gz`.

### Testnet

To make a testnet build, run:

```sh
$ ./rebar3 as testnet tar
```

The tarball is created at `_build/testnet/rel/bigfile/bigfile-x.y.z.tar.gz`.

You can join the public testnet now:

```
./bin/start peer testnet.thebigfile.info
```

We recommend you do not use your mainnet mining address on testnet. Also, do not join the
testnet from the mainnet machine.

### Starting New Weave

To start a new weave, create a new data directory

```sh
mkdir -p localnet_data_dir
```
,
create a wallet:

```sh
./bin/create-wallet localnet_data_dir
```
,
and run:

```sh
$ ./bin/start-localnet init data_dir <your-data-dir> mining_addr <your-mining-addr>
storage_module 0,<your-mining-addr> mine
```

The given address (if none is specified, one will be generated for you) will be assigned
`1_000_000_000_000` BIG in the new weave.

The network name will be `bigfile.localnet`. You can not start the same node again with the
init option unless you clean the data directory - you need to either restart with the
`start_from_block_index` option or specify a peer from the same Bigfile network via
`peer <peer>`. Note that the state is only persisted every 50 blocks so if you
restart the node without peers via `start_from_block_index` before reaching the height 50,
it will go back to the genesis block.

As with mainnet peers, each peer must be run in its own physical or virtual environment (e.g. on its own machine or in its own container or virtual machine). If you try to run two nodes within the same environment you will get an error like `Protocol 'inet_tcp': the name bigfile@127.0.0.1 seems to be in use by another Erlang node`

When POST'ing transactions to your localnet make sure to include the `X-Network: bigfile.localnet` header. If the header is omitted, the mainnet network will be assumed and the request will fail.

#### Configuring localnet

See the `localnet` section in [rebar.config](rebar.config) for instructions on changing
network constants for your localnet.

# Contributing

Make sure to have the build requirements installed.

Clone the repo and initialize the Git submodules:

```sh
$ git clone --recursive https://github.com/thebigfilecom/bigfile.git
```

## Running the tests

```sh
$ bin/test
```

## Running a shell

```sh
$ bin/shell
```

`bin/test` and `bin/shell` launch two connected Erlang VMs in distributed mode. The master VM runs an HTTP server on the port 1984. The slave VM uses the port 1983. The data folders are `data_test_master` and `data_test_slave` respectively. The tests that do not depend on two VMs are run against the master VM.

Run a specific test (the shell offers autocompletion):

```sh
(master@127.0.0.1)1> eunit:test(big_fork_recovery_tests:height_plus_one_fork_recovery_test_()).
```

If it fails, the nodes keep running so you can inspect them through Erlang shell or HTTP API.
The logs from both nodes are collected in `logs/`. They are rotated so you probably want to
consult the latest modified `master@127.0.0.1.*` and `slave@127.0.0.1.*` files first - `ls -lat
logs/`.

See [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

# HTTP API

You can find documentation regarding our HTTP interface [here](http_iface_docs.md).

# Contact

If you have questions or comments about Arweave you can get in touch by
finding us on [Twitter](https://x.com/thebigfile), [Reddit](https://www.reddit.com/r/TheBigFile/), [Discord](https://discord.com/invite/nyTAmMntqp) or by
emailing us at team@thebigfile.com.


For more information about the Arweave project visit [https://thebigfile.com](https://thebigfile.com/)
or have a look at our [yellow paper]().

# License

The Arweave project is released under GNU General Public License v2.0.
See [LICENSE](LICENSE.md) for full license conditions.
