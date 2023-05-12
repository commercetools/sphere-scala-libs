# Release process

How to create a new [release](../../releases).

## Releasing

The release process is automated thanks to:
- https://github.com/djspiewak/sbt-github-actions#integration-with-sbt-ci-release
- https://github.com/olafurpg/sbt-ci-release

To release, push a git tag:

```
git tag -a v0.1.0 -m "v0.1.0"
git push origin v0.1.0
```
Note that the tag version MUST start with `v`.

Wait for the [CI pipeline](../../actions) to release the new version. Publishing the artifacts on maven central can take time.

## Updating the release notes

Open the [releases](../../releases). A draft should already be prepared.

Edit the draft release to set the released version. Complete the release notes if necessary. And save it.

## Troubleshooting

If the `Publish Artifacts` job fails, it might be caused by an expired GPG signing key.
In that case you'll find the following entries in the job log:
```text
[info] gpg: no default secret key: No secret key
[info] gpg: signing failed: No secret key
```

To resolve the issue, you have to rotate the GPG signing key.
Follow the instructions, found [here](https://github.com/sbt/sbt-ci-release#gpg).

* for real name, use `sbt-ci-release bot`
* for email address, use `info@commercetools.com`

If the programmatic public key publishing fails for you (`gpg: keyserver send failed: Server indicated a failure`), you can publish the public key manually.
Use the forms on https://keyserver.ubuntu.com/ and http://pgp.mit.edu:11371/.
It's important to add the public key to both key servers!

Finally, update the `PGP_PASSPHRASE` and `PGP_SECRET` secrets in the [repository settings](https://github.com/commercetools/sphere-scala-libs/settings/secrets/actions).

_There is no need to save the key details anywhere else, because it's only used for signing and will never be verified.
If the credentials are lost, we can just generate a new key._

_Since the default key expiration timeout is 2 full years, it is not necessary to set up any rotation reminders.
The publishing failure and this troubleshooting guide should be enough to quickly resolve the problem._
