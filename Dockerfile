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
RUN stack install --dependencies-only --test --no-run-tests
COPY promote /src/promote/
COPY restylers /src/restylers/
RUN stack install --pedantic --test

RUN curl -sL https://get.docker.com/ | sh

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    awscli \
    ca-certificates \
    curl \
    git \
    gnupg \
    jq \
    locales \
    netbase \
    ruby-full && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN gem install jwt

COPY --from=builder /usr/bin/docker /bin/docker
COPY --from=builder /root/.local/bin/promote /bin/restyled-promote
COPY --from=builder /root/.local/bin/restylers /bin/restyled-restylers
COPY files/ /

ENV GIT_AUTHOR_NAME=Restyled.io
ENV GIT_AUTHOR_EMAIL=commits@restyled.io
ENV GIT_COMMITTER_NAME=Restyled.io
ENV GIT_COMMITTER_EMAIL=commits@restyled.io

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT ["restyled"]
