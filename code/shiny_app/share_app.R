#install.packages('rsconnect')

rsconnect::setAccountInfo(name='a-henderson91',
													token='16D6EF4D562A4F86527C81D795FDC7C6',
													secret='m0ac0IdDC0I3mFLsTUY6s3emy7DSUhUwCFrPrLR5')

rsconnect::deployApp('/Users/lsh1510922/Documents/COVID-Collateral/code/shiny_app/covid_collateral_shiny')
