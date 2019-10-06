
.deploy:
	git clone git@github.com:jannickj/elm-accessible-dropdown $@
	cd .deploy && git checkout gh-pages

deploy: .deploy
	elm make src/Main.elm --debug --output .deploy/index.html
	cd .deploy \
		&& git add . \
		&& git commit -m "new version"
		&& git push