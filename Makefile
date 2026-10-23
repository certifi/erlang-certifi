.PHONY: update

# Mozilla's server-authentication roots, published by the curl project.
update:
	@set -eu; \
	tmpdir=$$(mktemp -d priv/cacerts.XXXXXX); \
	trap 'rm -f "$$tmpdir/cacerts.pem" "$$tmpdir/cacert.pem.sha256" "$$tmpdir/cacerts.p7b"; rmdir "$$tmpdir"' EXIT; \
	trap 'exit 1' HUP INT TERM; \
	curl --fail --silent --show-error --location --proto '=https' --proto-redir '=https' \
		--output "$$tmpdir/cacerts.pem" https://curl.se/ca/cacert.pem; \
	curl --fail --silent --show-error --location --proto '=https' --proto-redir '=https' \
		--output "$$tmpdir/cacert.pem.sha256" https://curl.se/ca/cacert.pem.sha256; \
	expected=$$(awk 'NR == 1 {print $$1}' "$$tmpdir/cacert.pem.sha256"); \
	actual=$$(openssl dgst -sha256 "$$tmpdir/cacerts.pem" | awk '{print $$NF}'); \
	if [ "$$expected" != "$$actual" ]; then \
		echo 'CA bundle SHA-256 mismatch; keeping the existing bundle.' >&2; \
		exit 1; \
	fi; \
	grep -q '^-----BEGIN CERTIFICATE-----$$' "$$tmpdir/cacerts.pem"; \
	openssl crl2pkcs7 -nocrl -certfile "$$tmpdir/cacerts.pem" -out "$$tmpdir/cacerts.p7b"; \
	chmod 644 "$$tmpdir/cacerts.pem"; \
	mv "$$tmpdir/cacerts.pem" priv/cacerts.pem
