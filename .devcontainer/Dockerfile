# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.134.1/containers/alpine/.devcontainer/base.Dockerfile
ARG PYTHON_VERSION="3.8"
ARG OCAML_VERSION="4.11"
FROM python:${PYTHON_VERSION}-buster as base-python

# https://pythonspeed.com/articles/multi-stage-docker-python/
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN python -m pip install --upgrade pip
# For faster instalation
RUN pip install wheel
# Textbook generation requirements
RUN pip install jupyter-book "jupytext==1.7.1" pandas matplotlib "sphinxcontrib-bibtex<2.0.0"
RUN pip install --no-cache-dir --no-warn-script-location jupyter-book "jupytext==1.7.1" pandas matplotlib "sphinxcontrib-bibtex<2.0.0" numpy jupyter requests dash


# Once this gets fixed: https://discuss.ocaml.org/t/psa-ocaml-opam2-docker-images-updated-to-4-08-0-release/3948
# We can use smaller image
# FROM ocaml/opam2:debian-10
# This is fixed, use newer one, but as of 18.11.2020, ocaml 4.13 is not yet supported
FROM ocurrent/opam:debian-10-ocaml-${OCAML_VERSION}
# Redeclare arg: https://docs.docker.com/engine/reference/builder/#understand-how-arg-and-from-interact
ARG OCAML_VERSION

# m4 needs sudo: https://hub.docker.com/repository/registry-1.docker.io/j00sko/programiranje1/builds/1ca4dc84-ad7f-4ca4-af49-4756f8f6e02e
USER root
RUN apt-get update
RUN apt-get install -qq -yy libgmp-dev libzmq3-dev m4 perl pkg-config zlib1g-dev
USER opam

# Needed for merlin
RUN opam update && opam depext --yes --install jupyter ocaml-lsp-server ocamlformat utop
RUN opam user-setup install
RUN /home/opam/.opam/${OCAML_VERSION}/bin/ocaml-jupyter-opam-genspec

# Copy installed Python
COPY --from=base-python /opt/venv /opt/venv
COPY --from=base-python /usr/local/bin /usr/local/bin
COPY --from=base-python /usr/local/lib /usr/local/lib
ENV LD_LIBRARY_PATH='/lib:/usr/lib:/usr/local/lib'

# Make sure we use the virtualenv:
ENV PATH="/opt/venv/bin:$PATH"

# Get the correct OCaml
# This is a manual version of: RUN eval $(opam env)
ENV OPAM_SWITCH_PREFIX='/home/opam/.opam/'${OCAML_VERSION}
ENV CAML_LD_LIBRARY_PATH='/home/opam/.opam/'${OCAML_VERSION}'/lib/stublibs:Updated by package ocaml'
ENV OCAML_TOPLEVEL_PATH='/home/opam/.opam/'${OCAML_VERSION}'/lib/toplevel'
ENV MANPATH="$MANPATH"':/home/opam/.opam/'${OCAML_VERSION}'/man'
ENV OCAML_DOCKER_PATH='/home/opam/.opam/'${OCAML_VERSION}'/bin'
ENV PATH="$OCAML_DOCKER_PATH:$PATH"

# Install Jupyter kernel for OCaml
RUN jupyter kernelspec install --name ocaml-jupyter "$(opam var share)/jupyter" --user

# This enables us to install additional Python libraries
USER root
# Expose Jupyter port
EXPOSE 8888