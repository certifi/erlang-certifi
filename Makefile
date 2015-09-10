CURL_BIN=$(shell which curl)

$(if $(CURL_BIN),,$(warning "Warning: curl not found in your path."))


CERTIFI_URL=https://mkcert.org/generate/
CA_BUNDLE=cacerts.pem
CA_SRC=src/certifi_pemcerts.erl.src
CA_OUT=src/certifi_pemcerts.erl

mkcert:
	@curl -o $(CA_BUNDLE) https://mkcert.org/generate/
	@cat $(CA_SRC) \
		| head -n `grep -n "%% GENERATED" $(CA_SRC) | cut -d : -f 1` \
		> $(CA_OUT)
	@sed -e '/^#/d' $(CA_BUNDLE) >> $(CA_OUT)
	@cat $(CA_SRC) \
		| tail -n +`grep -n "%% GENERATED" $(CA_SRC) | cut -d : -f 1`  \
		>> $(CA_OUT)
	mv $(CA_BUNDLE) priv/
