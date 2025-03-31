# Changelog

## [Unreleased]

### Fixed
- Fixed issue with `missing_analysis$imputation_recommendations` returning NULL. Now the imputation recommendations are always generated and included in the result object, ensuring downstream code can reliably access them.

## [0.1.0] - Initial Release
- Initial package setup
- Added missing data analysis functionality
- Added imputation recommendations