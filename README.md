# NeuralSignalsOfPerformanceMonitoring
This repo contains the code for the simulations reported in the manuscript.

The file randomwalk_w_posterroronly.R implements a bounded accumulation model with additional post-decisional accumulation. The free parameter "only_post_err" indicates whether post-decisional processing operates the same as pre-decisional processing (i.e., the “continuation of pre-decisional accumulation” model) or only accumulates error evidence (i.e., the “selective error evidence” model). The file main.R runs the different simulations reported in the manuscript.
Note that this function was built for simulation purposes and is terribly slow, so it is not recommended to use this for actually fitting data. If you want to do so, one option is to code this function in the rcpp package (as done here: https://github.com/kdesende/dynamic_influences_on_static_measures) which considerably speeds up simulation time.
