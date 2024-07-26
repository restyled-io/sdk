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

## Examples

### Build and run an individual Restylers' tests

(Within `github.com/restyled-io/restylers`)

```console
restyled restylers prettier
```

### Upload a manifest of all restylers as `dev` and test it

```console
restyled promote --file restylers.yaml dev
```

### Download a manifest by name (`dev`) and test it

```console
restyled promote dev
```

### Download a manifest (`dev`), test it, and upload it as `staging`

```console
restyled promote dev staging
```

### Run the above, removing pulled images after use

```console
restyled promote dev staging --restyle-cmd "restyle --image-cleanup"
```

### Run the above, with a locally-developed `restyle` and debug logging

```console
restyled promote dev staging --restyle-cmd \
  "stack --stack-yaml '$PWD/../restyler/stack.yaml' exec -- restyle --debug"
```

## As GitHub Action

### Test an already-built Restyler image and create a manifest

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

### Publish a manifest without testing it

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
