FROM registry.access.redhat.com/ubi9/ubi

MAINTAINER Anthony Green <green@moxielogic.com>

ENV LC_ALL=C.utf8 \
    LANG=C.utf8 \
    LANGUAGE=C.utf8 \
    SBCL_VERSION=2.3.10 \
    GREEN_ORB_VERSION=0.2.4 \
    PATH=/opt/daily-price-depot-droid/.local/bin:$PATH \
    HOME=/opt/daily-price-depot-droid

RUN dnf -y install bzip2 git make gcc

RUN curl -L -O "https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2" \
    && tar -xf sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 \
    && cd sbcl-${SBCL_VERSION}-x86-64-linux \
    && ./install.sh --prefix=/usr/local \
    && cd .. \
    && rm -rf sbcl-${SBCL_VERSION}-x86-64-linux-binary.tar.bz2 sbcl-${SBCL_VERSION}-x86-64-linux

RUN useradd -r -u 1000  -m -d /opt/daily-price-depot-droid -s /bin/bash daily-price-depot-droid

WORKDIR /opt/daily-price-depot-droid
COPY . .
RUN chown -R daily-price-depot-droid /opt/daily-price-depot-droid

USER 1000

RUN git clone --depth=1 https://github.com/ocicl/ocicl.git; cd ocicl; make; make install; ocicl version; ocicl setup > ~/.sbclrc \
    && echo "(push (uiop:getcwd) asdf:*central-registry*)" >> ~/.sbclrc \
    && echo "(setf ocicl-runtime:*verbose* t)" >> ~/.sbclrc \
    && echo "(setf ocicl-runtime:*download* t)" >> ~/.sbclrc \
    && sbcl --non-interactive --eval "(quit)" \
    && cd .. \
    && ocicl install \
    && sbcl --userinit /opt/daily-price-depot-droid/.sbclrc --eval '(asdf:load-system :daily-price-depot-droid)' --eval '(quit)'

RUN curl -L -O "https://github.com/atgreen/green-orb/releases/download/v${GREEN_ORB_VERSION}/green-orb-${GREEN_ORB_VERSION}-linux-amd64.tar.gz" \
    && tar xf green-orb-${GREEN_ORB_VERSION}-linux-amd64.tar.gz \
    && rm green-orb-${GREEN_ORB_VERSION}-linux-amd64.tar.gz

RUN chmod -R go+rwx /opt/daily-price-depot-droid

CMD ./orb sbcl --userinit /opt/daily-price-depot-droid/.sbclrc --eval '(asdf:load-system :daily-price-depot-droid)' --eval '(daily-price-depot-droid:pull-daily)'
