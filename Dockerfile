FROM fpco/stack-build-small:lts-22.28 AS builder
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
RUN stack install --dependencies-only --test --no-run-tests
COPY promote /src/promote/
COPY restylers /src/restylers/
RUN stack install --pedantic --test

RUN curl -sL https://get.docker.com/ | sh

FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    gnupg \
    jq \
    locales \
    netbase \
    ruby-full \
    unzip && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN gem install jwt

# AWS CLIv2
RUN \
  cd /tmp && \
  curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" && \
  unzip awscliv2.zip && \
  ./aws/install && \
  rm -rf aws awscliv2.zip

# Docker
COPY --from=builder /usr/bin/docker /bin/docker
COPY --from=docker/buildx-bin /buildx /usr/libexec/docker/cli-plugins/docker-buildx
RUN docker buildx version

COPY --from=builder /root/.local/bin/promote /bin/restyled-promote
COPY --from=builder /root/.local/bin/restylers /bin/restyled-restylers
COPY files/ /

ENV GIT_AUTHOR_NAME=Restyled.io
ENV GIT_AUTHOR_EMAIL=commits@restyled.io
ENV GIT_COMMITTER_NAME=Restyled.io
ENV GIT_COMMITTER_EMAIL=commits@restyled.io

# Restyle
RUN curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/restyled-io/restyler/main/install | sh

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT ["restyled"]
