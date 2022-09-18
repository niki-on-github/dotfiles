ARG LATEX_IMAGE_TAG=latest
FROM pandoc/latex:$LATEX_IMAGE_TAG

RUN apk add --no-cache \
    chromium \
    curl \
    fontconfig \
    freetype \
    graphviz \
    nodejs \
    librsvg \
    npm \
    openjdk17-jre \
    py3-pip \
    python3 \
    texmf-dist-fontsextra \
    ttf-dejavu \
    ttf-droid \
    ttf-droid-nonlatin

RUN fc-cache -fv

ENV PLANTUML_VERSION 1.2022.7

# see https://plantuml.com/command-line
RUN wget "http://downloads.sourceforge.net/project/plantuml/${PLANTUML_VERSION}/plantuml.${PLANTUML_VERSION}.jar" -O /usr/bin/plantuml.jar \
  && printf '#!/bin/sh\njava -jar /usr/bin/plantuml.jar -Sdpi=800 $@' > /usr/bin/plantuml \
  && chmod +x /usr/bin/plantuml

ENV PLANTUML_BIN="/usr/bin/plantuml"
# ENV PLANTUML_FILETYPE="svg"

RUN pip install --no-cache-dir --upgrade pip \
  && pip install --no-cache-dir pandoc-plantuml-filter \
  && pip install --no-cache-dir pandoc-include

RUN npm install --global mermaid-filter

ENV PUPPETEER_SKIP_CHROMIUM_DOWNLOAD="true"
ENV CHROMIUM_PATH="/usr/bin/chromium-browser"
ENV PUPPETEER_EXECUTABLE_PATH="${CHROMIUM_PATH}"

ENV MERMAID_BIN=/usr/local/bin/mmdc

RUN tlmgr option repository http://mirror.ctan.org/systems/texlive/tlnet \
    && tlmgr update  --self \
    && tlmgr install \
        adjustbox \
        awesomebox \
        babel-german \
        background \
        bidi \
        collectbox \
        csquotes \
        epstopdf \
        everypage \
        filehook \
        fontawesome5 \
        fontaxes \
        footmisc \
        footnotebackref \
        framed \
        fvextra \
        koma-script \
        letltxmacro \
        lineno \
        ly1 \
        mdframed \
        merriweather \
        mweights \
        needspace \
        pagecolor \
        pdftexcmds \
        sectsty \
        sourcesanspro \
        sourcecodepro \
        titlesec \
        titling \
        ucharcat \
        ulem \
        unicode-math \
        upquote \
        xecjk \
        xurl \
        zref

RUN mkdir -p /opt/pandoc/templates && \
    wget -O - https://github.com/Wandmalfarbe/pandoc-latex-template/releases/download/v2.0.0/Eisvogel-2.0.0.tar.gz | \
    tar zxvf - -C /opt/pandoc/templates

