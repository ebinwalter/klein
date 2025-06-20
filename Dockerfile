FROM rustlang/rust:nightly-bullseye-slim as rst

WORKDIR /app
COPY . .
RUN cargo build
RUN pwd
RUN ls

FROM openjdk:11.0.11-jre-slim as mars
COPY --from=rst /app /app
COPY tests /app/tests

WORKDIR /app
RUN chmod +x tests/run_tests.sh
WORKDIR /app/tests
ENTRYPOINT ["/bin/bash", "./run_tests.sh"]
