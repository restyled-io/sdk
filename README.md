# Restyled SDK

Toolkit for Developing and/or Operating Restyled

## Installation

```console
mkdir -p ~/.local/bin

curl -sL -o ~/.local/bin/restyled \
  https://raw.githubusercontent.com/restyled-io/sdk/main/restyled

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

```yaml
- name: Promote
  uses: restyled-io/sdk@main
  with:
    command: >-
      promote --yes --no-test --file restylers.yaml ${{ github.sha }}
  env:
    AWS_ACCESS_KEY_ID: ${{ secrets.aws_access_key_id }}
    AWS_SECRET_ACCESS_KEY: ${{ secrets.aws_secret_access_key }}
    AWS_DEFAULT_REGION: us-east-1
```

---

[LICENSE](./LICENSE)
