let main =
  (* math-ext.mc *)
  Boot.Intrinsics.add_external "exp" Float.exp;
  Boot.Intrinsics.add_external "log" Float.log;
  Boot.Intrinsics.add_external "atan" Float.atan;
  Boot.Intrinsics.add_external "sin" Float.sin;
  Boot.Intrinsics.add_external "cos" Float.cos;
  Boot.Intrinsics.add_external "atan2" Float.atan2;
  Boot.Intrinsics.add_external "pow" Float.pow;
  Boot.Intrinsics.add_external "sqrt" Float.sqrt;

  (* dist-ext.mc *)
  Boot.Intrinsics.add_external "externalExponentialSample" Owl_stats.exponential_rvs;
  Boot.Intrinsics.add_external "externalGammaLogPdf" Owl_stats.gamma_logpdf;
  Boot.Intrinsics.add_external "externalGammaSample" Owl_stats.gamma_rvs;
  Boot.Intrinsics.add_external "externalBinomialLogPmf" Owl_stats.binomial_logpdf;
  Boot.Intrinsics.add_external "externalBinomialSample" Owl_stats.binomial_rvs;
  Boot.Intrinsics.add_external "externalBetaLogPdf" Owl_stats.beta_logpdf;
  Boot.Intrinsics.add_external "externalBetaSample" Owl_stats.beta_rvs;
  Boot.Intrinsics.add_external "externalGaussianLogPdf" Owl_stats.gaussian_logpdf;
  Boot.Intrinsics.add_external "externalGaussianSample" Owl_stats.gaussian_rvs;
  Boot.Intrinsics.add_external "externalMultinomialLogPmf" Owl_stats.multinomial_logpdf;
  Boot.Intrinsics.add_external "externalMultinomialSample" Owl_stats.multinomial_rvs;
  Boot.Intrinsics.add_external "externalCategoricalSample" Owl_stats.categorical_rvs;
  Boot.Intrinsics.add_external "externalDirichletLogPdf" Owl_stats.dirichlet_logpdf;
  Boot.Intrinsics.add_external "externalDirichletSample" Owl_stats.dirichlet_rvs;
  Boot.Intrinsics.add_external "externalUniformContinuousSample" Owl_stats.uniform_rvs;
  Boot.Intrinsics.add_external "externalUniformDiscreteSample" Owl_stats.uniform_int_rvs;

  (* and so on for the rest of the externals declared in the original MExpr file... *)

  ()
