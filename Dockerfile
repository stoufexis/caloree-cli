FROM haskell:9.0.2-buster as build

WORKDIR /opt/

COPY . .

RUN stack install .

FROM alpine:latest

ENV USER=caloree

COPY --from=build /root/.local/bin/caloree-cli-exe /opt/$USER/caloree

RUN addgroup -S $USER \
    && adduser -S -g $USER $USER \
    && chown -R $USER:$USER /home/$USER \
        /opt/$USER

USER $USER

CMD [ "/opt/caloree/caloree" ]
