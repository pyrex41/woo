#!/bin/sh
set -eu

mkdir -p t/certs
cd t/certs

openssl genrsa -out localCA.key 2048
openssl req -batch -new -key localCA.key -out localCA.csr \
  -subj "/C=JP/ST=Tokyo/L=Chuo-ku/O=\"Woo\"/OU=Development/CN=localhost"
cat > localCA.csx <<'EXTENSIONS'
basicConstraints = critical, CA:TRUE, pathlen:0
keyUsage = critical, keyCertSign, cRLSign
subjectKeyIdentifier = hash
EXTENSIONS
openssl x509 -req -days 3650 -signkey localCA.key -in localCA.csr -extfile localCA.csx -out localCA.crt
openssl x509 -text -noout -in localCA.crt
openssl genrsa -out localhost.key 2048
openssl req -batch -new -key localhost.key -out localhost.csr \
  -subj "/C=JP/ST=Tokyo/L=Chuo-ku/O=\"Woo\"/OU=Development/CN=localhost"
cat > localhost.csx <<'EXTENSIONS'
basicConstraints = critical, CA:FALSE
keyUsage = critical, digitalSignature, keyEncipherment
extendedKeyUsage = serverAuth
subjectAltName = DNS:localhost, DNS:localhost.localdomain, IP:127.0.0.1, DNS:app, DNS:app.localdomain
EXTENSIONS
openssl x509 -req -days 1825 -CA localCA.crt -CAkey localCA.key -CAcreateserial -in localhost.csr -extfile localhost.csx -out localhost.crt

openssl verify -CAfile localCA.crt localhost.crt
