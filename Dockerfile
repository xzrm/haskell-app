FROM haskell:9.0.2-slim-buster as build

RUN mkdir -p /opt/build

RUN apt-get update

WORKDIR /opt/build

COPY stack.yaml package.yaml stack.yaml.lock ./

RUN stack build --only-dependencies

COPY . .
RUN stack build

RUN mv "$(stack path --local-install-root)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
FROM ubuntu:20.04 as app

RUN mkdir -p /opt/app

WORKDIR /opt/app

RUN apt-get update && apt-get install -y \
    ca-certificates \
    sqlite3 libsqlite3-dev \
    libnuma-dev 

COPY --from=build /opt/build/bin .

ENV ENV_TYPE=pro

COPY rates.db .

VOLUME /opt/app/rates.db

EXPOSE 8081

CMD ["/opt/app/my-project-exe", "8081"]