# Restyled SDK

Toolkit for Developing and/or Operating Restyled

## Installation

```console
mkdir -p ~/.local/bin

curl -sL -o ~/.local/bin/restyled \
  https://raw.githubusercontent.com/restyled-io/sdk/main/restyled

chmod +x ~/.local/bin/restyled

export PATH=$HOME/.local/bin:$PATH
```

## Updates

```console
docker pull restyled/sdk:main
```

## Usage

```hs
restyled <subcommand> [option...]
```

## As GitHub Action

```yaml
- name: Test
  uses: restyled-io/sdk@main
  with:
    command: >-
      restylers
      --sha ${{ github.sha }}
      --no-build
      --write restyler.yaml
      ${{ matrix.restyler }}
```

---

[LICENSE](./LICENSE)
