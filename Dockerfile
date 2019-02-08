FROM fpco/stack-build:lts-11.19

RUN apt-get update && apt-get install -y python3-pip libgmp-dev libmagic-dev libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libblas-dev liblapack-dev gcc g++ && \
    rm -rf /var/lib/apt/lists/*

RUN pip3 install -U jupyter

RUN adduser --disabled-password --gecos "Default user" --uid 1000 jovyan

USER 1000
ENV LANG en_US.UTF-8
ENV HOME /home/jovyan
WORKDIR ${HOME}

RUN stack --resolver lts-11.19 setup && stack config set system-ghc --global true

COPY --chown=1000 docker-stack.yaml stack.yaml
COPY --chown=1000 ihaskell ihaskell

RUN mkdir stochaskell
COPY --chown=1000 stochaskell/stochaskell.cabal stochaskell/stochaskell.cabal
RUN stack setup && stack build --only-snapshot

COPY --chown=1000 stochaskell stochaskell
RUN stack build && stack install

COPY --chown=1000 *.ipynb ./

ENV PATH $(stack path --local-install-root)/bin:$(stack path --snapshot-install-root)/bin:$(stack path --compiler-bin):${HOME}/.local/bin:${PATH}
RUN ihaskell install --stack
CMD ["jupyter", "notebook", "--ip", "0.0.0.0"]
