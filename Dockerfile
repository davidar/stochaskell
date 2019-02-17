FROM fpco/stack-build:lts-11.19

RUN apt-get update && apt-get install -y \
      gcc g++ \
      libblas-dev \
      libcairo2-dev \
      libgmp-dev \
      liblapack-dev \
      libmagic-dev \
      libpango1.0-dev \
      libtinfo-dev \
      libzmq3-dev \
      python-dev \
      python3-pip \
      virtualenv \
      && rm -rf /var/lib/apt/lists/*

RUN pip3 install -U jupyter

RUN adduser --disabled-password --gecos "Default user" --uid 1000 jovyan

USER 1000
ENV LANG en_US.UTF-8
ENV HOME /home/jovyan
WORKDIR ${HOME}

RUN stack --resolver lts-11.19 setup && stack config set system-ghc --global true

RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
ENV NVM_DIR ${HOME}/.nvm
RUN . ${NVM_DIR}/nvm.sh && nvm install 0.10.26

RUN mkdir stochaskell

COPY --chown=1000 stochaskell/webchurch stochaskell/webchurch
RUN cd stochaskell/webchurch && ./compile.sh && cd

COPY --chown=1000 stochaskell/edward stochaskell/edward
COPY --chown=1000 stochaskell/pymc3 stochaskell/pymc3
COPY --chown=1000 stochaskell/Makefile stochaskell/Makefile
RUN make -C stochaskell env

COPY --chown=1000 stochaskell/cmdstan stochaskell/cmdstan
RUN make -C stochaskell/cmdstan build

COPY --chown=1000 docker-stack.yaml stack.yaml
COPY --chown=1000 ihaskell ihaskell
COPY --chown=1000 stochaskell/package.yaml stochaskell/package.yaml
RUN stack setup && stack build --only-snapshot
ENV PATH $(stack path --local-install-root)/bin:$(stack path --snapshot-install-root)/bin:$(stack path --compiler-bin):${HOME}/.local/bin:${PATH}

COPY --chown=1000 stochaskell stochaskell
RUN stack build && stack install
RUN ihaskell install --stack
ENV STOCHASKELL_DIRECTORY ${HOME}/stochaskell

COPY --chown=1000 docs/*.ipynb LICENSE ./
CMD ["jupyter", "notebook", "--ip", "0.0.0.0"]
