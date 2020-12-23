FROM fpco/stack-build-small:lts-16.12 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /src/bin
WORKDIR /src

COPY stack.yaml /src/stack.yaml
COPY promote/package.yaml /src/promote/package.yaml
COPY restylers/package.yaml /src/restylers/package.yaml
RUN stack install --dependencies-only
COPY promote /src/promote/
COPY restylers /src/restylers/
RUN stack install

# Docker client
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates=20190110~18.04.1 \
    locales=2.27-3ubuntu1.2 \
    netbase=5.4 && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/bin/docker /bin/docker
COPY --from=builder /root/.local/bin/promote /bin/restyled-promote
COPY --from=builder /root/.local/bin/restylers /bin/restyled-restylers
COPY subcommands/ /src/bin
RUN \
  for subcommand in /src/bin/*; do \
    cp "$subcommand" /bin/restyled-"$(basename "$subcommand")"; \
  done
COPY restyled /bin/restyled
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT ["restyled"]
