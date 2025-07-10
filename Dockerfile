# ─────────────────────────── builder stage ────────────────────────────────
FROM rocker/r-ver:4.4.2 AS builder

ENV DEBIAN_FRONTEND=noninteractive \
HF_HOME=/opt/hf-cache

# Install system dependencies
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
        curl git ca-certificates \
        libcurl4-openssl-dev libssl-dev libxml2-dev \
        libopenblas-dev liblapack-dev libnode-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Install R packages
COPY renv.lock renv.lock
RUN R -q -e "install.packages('renv', repos='https://cloud.r-project.org'); \
  renv::restore()"

# ──────────────────────── python shared stage ─────────────────────────────
FROM python:3.12-slim AS python-shared
# Python here is built with --enable-shared by default

# ─────────────────────────── runtime stage ────────────────────────────────
FROM rocker/r-ver:4.4.2

LABEL org.opencontainers.image.title="KWALLM: Text analysis with LLM" \
      org.opencontainers.image.version="See version tag at https://github.com/KennispuntTwente/tekstanalyse_met_llm/pkgs/" \
      org.opencontainers.image.description="Application for (automated) qualitative text analysis with large language models (LLMs)." \
      org.opencontainers.image.authors="Luka Koning <l.koning@kennispunttwente.nl>" \
      org.opencontainers.image.licenses="AGPL-3.0-only" \
      org.opencontainers.image.vendor="Kennispunt Twente" \
      org.opencontainers.image.source="https://github.com/KennispuntTwente/tekstanalyse_met_llm" \
      org.opencontainers.image.base.name="rocker/r-ver:4.4.2" \
      org.opencontainers.image.ref.name="rocker/r-ver"

ENV TZ=Europe/Amsterdam

# Minimal system dependencies + Python runtime
RUN apt-get update -qq && \
  apt-get install -y --no-install-recommends \
    libcurl4 libssl3 libxml2 pandoc pkg-config cmake curl unzip && \
  apt-get clean && rm -rf /var/lib/apt/lists/*

# Copy Python from shared build (with --enable-shared)
COPY --from=python-shared /usr/local /usr/local

# Create non-root user
RUN useradd -ms /bin/bash appuser

# Copy R library
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy application files
WORKDIR /home/appuser/app
COPY --chown=appuser:appuser R/ R/
COPY --chown=appuser:appuser Dockerfile-app.R app.R
COPY --chown=appuser:appuser www/ www/
COPY --chown=appuser:appuser language/ language/
COPY --chown=appuser:appuser LICENSE.md LICENSE.md
COPY --chown=appuser:appuser pyproject.toml pyproject.toml
COPY --chown=appuser:appuser uv.lock uv.lock

# Ensure app directory and contents are owned by appuser and fully accessible
RUN chown -R appuser:appuser /home/appuser/app && \
    chmod -R u+rwX /home/appuser/app

# Switch to non-root user
USER appuser

# Cache model by running load script once
# (This also installs the Python environment)
ENV HF_HUB_OFFLINE=0
RUN Rscript -e "\
  reticulate:::uv_exec('sync');\
  source('R/gliner_load_model.R');\
  source('R/async_message_printer.R');\
  gliner_load_model();\
  reticulate:::uv_exec('cache clean');\
"
ENV HF_HUB_OFFLINE=1

# Limit OpenMP to 1 thread; leads to better performance for GLiNER in Docker env
ENV OMP_NUM_THREADS=1

# Run the application
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/home/appuser/app', host='0.0.0.0', port=3838)"]
