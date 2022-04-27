cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in a8d8f9ebfbe4e59a8a7c8f5292f46c45c42ac7476fe3745d4aacf60d67b81368#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral 32ea59264ac0e1ad87af4a68d0f1451cf56d3e869e990d5a07add83d90836371#1 \
    --required-signer-hash af27aa9d9dc1baeae6afc55dd8d28d52db8de051949dc4c73d4eca05 \
    --invalid-before 56717801 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
