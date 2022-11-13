let conv x =
  Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 x)

let main =
  (* math-ext.mc *)
  Boot.Intrinsics.Ext.add_external (conv "exp") Float.exp;
  Boot.Intrinsics.Ext.add_external (conv "log") Float.log;
  Boot.Intrinsics.Ext.add_external (conv "atan") Float.atan;
  Boot.Intrinsics.Ext.add_external (conv "sin") Float.sin;
  Boot.Intrinsics.Ext.add_external (conv "cos") Float.cos;
  Boot.Intrinsics.Ext.add_external (conv "atan2") Float.atan2;
  Boot.Intrinsics.Ext.add_external (conv "pow") Float.pow;
  Boot.Intrinsics.Ext.add_external (conv "sqrt") Float.sqrt;

  (* dist-ext.mc *)
  Boot.Intrinsics.Ext.add_external (conv "externalExponentialSample") Owl_stats.exponential_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalGammaLogPdf") Owl_stats.gamma_logpdf;
  Boot.Intrinsics.Ext.add_external (conv "externalGammaSample") Owl_stats.gamma_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalBinomialLogPmf") Owl_stats.binomial_logpdf;
  Boot.Intrinsics.Ext.add_external (conv "externalBinomialSample") Owl_stats.binomial_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalBetaLogPdf") Owl_stats.beta_logpdf;
  Boot.Intrinsics.Ext.add_external (conv "externalBetaSample") Owl_stats.beta_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalGaussianLogPdf") Owl_stats.gaussian_logpdf;
  Boot.Intrinsics.Ext.add_external (conv "externalGaussianSample") Owl_stats.gaussian_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalMultinomialLogPmf") Owl_stats.multinomial_logpdf;
  Boot.Intrinsics.Ext.add_external (conv "externalMultinomialSample") Owl_stats.multinomial_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalCategoricalSample") Owl_stats.categorical_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalDirichletLogPdf") Owl_stats.dirichlet_logpdf;
  Boot.Intrinsics.Ext.add_external (conv "externalDirichletSample") Owl_stats.dirichlet_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalUniformContinuousSample") Owl_stats.uniform_rvs;
  Boot.Intrinsics.Ext.add_external (conv "externalUniformDiscreteSample") Owl_stats.uniform_int_rvs;

  (* and so on for the rest of the externals declared in the original MExpr file... *)

  ()
