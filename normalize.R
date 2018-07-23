need_oversample = TRUE

if (need_oversample) {
  sampler <- oversampling(feature, output)
  Y <- sampler$Y
  feature_norm <- featureNormalize(sampler$X)
} else {
  Y <- output
  feature_norm <- featureNormalize(feature)
}

m <- nrow(Y)
