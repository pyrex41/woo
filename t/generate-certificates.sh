#!/bin/sh
set -eu

certificate_dir=${1:-t/certs}
mkdir -p "$certificate_dir"
cd "$certificate_dir"

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

# A separate root with pathlen=1 supplies a real intermediate-chain fixture
# for TLS trust and fullchain tests. These development keys are generated
# locally and are not committed.
openssl genrsa -out chain-root.key 2048
openssl req -batch -new -x509 -days 3650 -key chain-root.key -out chain-root.crt \
  -subj "/C=US/O=Woo Test/CN=Woo Chain Root" \
  -addext "basicConstraints=critical,CA:TRUE,pathlen:1" \
  -addext "keyUsage=critical,keyCertSign,cRLSign" \
  -addext "subjectKeyIdentifier=hash"
openssl genrsa -out chain-intermediate.key 2048
openssl req -batch -new -key chain-intermediate.key -out chain-intermediate.csr \
  -subj "/C=US/O=Woo Test/CN=Woo Chain Intermediate"
cat > chain-intermediate.csx <<'EXTENSIONS'
basicConstraints=critical,CA:TRUE,pathlen:0
keyUsage=critical,keyCertSign,cRLSign
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid,issuer
EXTENSIONS
openssl x509 -req -days 1825 -in chain-intermediate.csr -CA chain-root.crt \
  -CAkey chain-root.key -CAcreateserial -out chain-intermediate.crt \
  -extfile chain-intermediate.csx
openssl genrsa -out chain-leaf.key 2048
openssl req -batch -new -key chain-leaf.key -out chain-leaf.csr \
  -subj "/C=US/O=Woo Test/CN=localhost"
cat > chain-leaf.csx <<'EXTENSIONS'
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth
subjectAltName=DNS:localhost,IP:127.0.0.1
EXTENSIONS
openssl x509 -req -days 825 -in chain-leaf.csr -CA chain-intermediate.crt \
  -CAkey chain-intermediate.key -CAcreateserial -out chain-leaf.crt \
  -extfile chain-leaf.csx
openssl verify -CAfile chain-root.crt -untrusted chain-intermediate.crt chain-leaf.crt
