##To run the generator

cabal repl

import Oracle.Deploy
writeOracleValidator `walletAddr`

```
Prelude Utils Oracle.OffChain> unsafeReadAddress "addr_test1qr8sdyxxppl03hy5mve8naemkfe3fw44pd65z8wgym0dft4grhtvmgrjcsj5wn9hqccztkn8x9ez4lt9y3eduhqqzs2qwpwscp"
Address {addressCredential = PubKeyCredential cf0690c6087ef8dc94db3279f73bb27314bab50b75411dc826ded4ae, addressStakingCredential = Just (StakingHash (PubKeyCredential a81dd6cda072c425474cb7063025da6731722afd652472de5c001414))}
```



