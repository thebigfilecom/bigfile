# Internal HTTP API for generating wallets and posting unsigned transactions

## **Warning** only use it if you really really know what you are doing.

These HTTP endpoints are only available if the `internal_api_secret` startup option is set when `bigfile-server` is started.

## Generate a wallet and receive its access code

- **URL**
  `/wallet`

- **Method**
  POST

- **Request Headers**
    * `X-Internal-Api-Secret` : must match `internal_api_secret`

#### Example Response

An access code which can be used to sign transactions via `POST /unsigned_tx`.

```javascript
{"wallet_access_code":"UEhkVh0LBqfIj60-EB-yaDSrMpR2_EytWrY0bGJc_AZaiITJ4PrzRZ_xaEH5KBD4"}
```

## POST unsigned transaction to the network

Post a transaction to be signed and sent to the network.

- **URL**
  `/unsigned_tx`

- **Method**
  POST

- **Request Headers**
   * `X-Internal-Api-Secret` : must match `internal_api_secret`

#### Data Parameter (Post body)

```javascript
{
    "last_tx": "",            // Base64 encoded ID of the last transaction made by this wallet.
    "target": "",             // Base64 encoded SHA256 hash of recipient's public key. Empty for data transactions.
    "quantity": "",           // Decimal string representation of the amount of sent BIG in wei. Empty for data transactions.
    "data": "",               // The Base64 encoded data being store in the transaction. Empty for transfer transactions.
    "reward": "",             // Decimal string representation of the mining reward BIG amount in wei.
    "wallet_access_code": ""  // The wallet access code as returned by the POST /wallet endpoint.
}
```


#### Example Response

A transaction ID (Base64 encoded hash of the signature).

```javascript
{"id": "F8ITA-zojpRtUNnULnKasJCHL46rcqQBpSyqBekWnF30S7GCd58LcIcOXhYnYL6U"}
```
