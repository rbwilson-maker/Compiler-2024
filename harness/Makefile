sources = *.sml */*.sml sources.mlb

driver:
	mlton -output driver sources.mlb

update-nj:
	molasses sources.mlb

clean:
	rm driver >/dev/null 2>&1 || true
