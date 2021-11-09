FROM quay.io/atgreen/daily-price-depot-droid-base:latest

USER 0
RUN rm /etc/localtime && ln -s /usr/share/zoneinfo/Canada/Eastern /etc/localtime
USER 1000

COPY . /tmp/src
ARG APP_VERSION=APP_VERSION
ENV APP_VERSION=${APP_VERSION}
RUN APP_SYSTEM_NAME=daily-price-depot-droid /usr/libexec/s2i/assemble
CMD DEV_BACKEND=slynk APP_SYSTEM_NAME=daily-price-depot-droid APP_EVAL="\"(daily-price-depot-droid:start-server)\"" /usr/libexec/s2i/run
