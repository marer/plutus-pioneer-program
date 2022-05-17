cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic 1097911063 \
  --change-address $(cat 01.addr) \
  --tx-in 7d754977d80b059220478f811d83684de50d3b7da48a9759b19594fe6a29f10a#0 \
  --tx-out "$(cat 02.addr) 10000000 lovelace" \
  --out-file tx1.body

cardano-cli transaction sign \
  --tx-body-file tx1.body \
  --signing-key-file 01.skey \
  --testnet-magic 1097911063 \
  --out-file tx1.signed

cardano-cli transaction submit \
  --testnet-magic 1097911063 \
  --tx-file tx1.signed