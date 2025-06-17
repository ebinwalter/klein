FROM rustlang/rust:nightly-bullseye-slim as rst

WORKDIR /app
COPY . .
RUN cargo build
RUN pwd
RUN ls

FROM eclipse-temurin:21-jdk-jammy as deps
COPY --from=rst /app /app
COPY tests /app/tests

WORKDIR /app
RUN chmod +x tests/run_tests.sh
WORKDIR /app/tests
RUN /usr/bin/bash run_tests.sh
