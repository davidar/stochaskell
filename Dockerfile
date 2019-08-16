FROM ubuntu:18.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl=7.58.* \
    g++=4:7.3.* \
    gcc=4:7.3.* \
    git=1:2.17.* \
    gnupg=2.2.* \
    libblas-dev=3.7.* \
    libc6-dev=2.27-* \
    libcairo2-dev=1.15.* \
    libffi-dev=3.2.* \
    libgmp-dev=2:6.1.* \
    liblapack-dev=3.7.* \
    libmagic-dev=1:5.32-* \
    libpango1.0-dev=1.40.* \
    libtinfo-dev=6.1-* \
    libzmq3-dev=4.2.* \
    make=4.1-* \
    python-dev=2.7.* \
    python3-pip=9.0.* \
    python3-setuptools=39.0.* \
    virtualenv=15.1.* \
    xz-utils=5.2.* \
    zlib1g-dev=1:1.2.* \
 && rm -rf /var/lib/apt/lists/*

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN pip3 install --no-cache-dir notebook==5.*

RUN adduser --disabled-password --gecos "Default user" --uid 1000 jovyan

USER 1000
ENV LANG en_US.UTF-8
ENV HOME /home/jovyan
WORKDIR ${HOME}

RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
ENV NVM_DIR ${HOME}/.nvm
ENV NODE_VERSION 0.10.26
RUN . ${NVM_DIR}/nvm.sh && nvm install ${NODE_VERSION}
ENV PATH ${NVM_DIR}/v${NODE_VERSION}/bin:${PATH}

RUN mkdir stochaskell

COPY --chown=1000 stochaskell/webchurch stochaskell/webchurch
WORKDIR ${HOME}/stochaskell/webchurch
RUN set -ex; . compile.sh
WORKDIR ${HOME}

COPY --chown=1000 stochaskell/edward stochaskell/edward
COPY --chown=1000 stochaskell/pymc3 stochaskell/pymc3
COPY --chown=1000 stochaskell/Makefile stochaskell/Makefile
RUN make -C stochaskell env && rm -rf .cache

COPY --chown=1000 stochaskell/cmdstan stochaskell/cmdstan
RUN make -C stochaskell/cmdstan build

RUN stack --resolver lts-13.26 setup \
 && rm -rf .stack/programs/*-linux/ghc-*{.tar.xz,/share}

COPY --chown=1000 stack.yaml stack.yaml
RUN echo '{}' > stochaskell/package.yaml \
 && stack build \
    ihaskell \
    ihaskell-aeson \
    ihaskell-blaze \
    ihaskell-charts \
    ihaskell-diagrams \
    ihaskell-gnuplot \
    ihaskell-hatex \
    ihaskell-juicypixels \
    ihaskell-magic \
#    ihaskell-plot \
#    ihaskell-static-canvas \
#    ihaskell-widgets \
 && stack clean \
 && rm -rf .stack/indices .stack-work/downloaded
COPY --chown=1000 stochaskell/package.yaml stochaskell/package.yaml
RUN stack build --only-snapshot && stack clean && rm -rf .stack/indices

COPY --chown=1000 Makefile Makefile
COPY --chown=1000 stochaskell stochaskell
RUN make install && stack clean && rm -rf .stack/indices
ENV STOCHASKELL_DIRECTORY ${HOME}/stochaskell

COPY --chown=1000 *.ipynb LICENSE ./
CMD ["jupyter", "notebook", "--ip", "0.0.0.0"]
