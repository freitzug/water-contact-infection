
sessionInfo()
set.seed(721)

#set wd to /code folder

#Run data prep
source("prep/01_paths_pkgs.R")
source("prep/02_funs.R")
source("prep/03_read.R")
source("prep/12_dict.R")
#source("prep/13_applylabs.R")
source("prep/14_lrts.R")
source("prep/15_bas.R")

#Run main
source("do/main/fig3_lake_dist.R")
source("do/main/fig4_gams.R")
source("do/main/fig5_composition.R")
source("do/main/fig6_var_sel.R")
source("do/main/fig7_forestplots_exp.R")
source("do/main/fig8_forestplot_inf.R")
source("do/main/fig9_rocs.R")

#Run s_figs
source("do/s_figs/s_fig1_histograms.R")
source("do/s_figs/s_fig2_quantile_plot.R")
source("do/s_figs/s_fig3_gam_gender.R")
source("do/s_figs/s_fig4_water_contact_no_occupation_model.R")
source("do/s_figs/s_fig5_occ_dom_rec_models.R")
source("do/s_figs/s_fig6_freq_model.R")
source("do/s_figs/s_fig7_duration_model.R")
source("do/s_figs/s_fig8_gam_infection_water_contact.R")
source("do/s_figs/s_fig9_gam_infected_dist.R")
source("do/s_figs/s_fig10_intensity_model.R")
source("do/s_figs/s_fig11_water_contact_obs_comp.R")
source("do/s_figs/s_fig12_reclassify_kk.R")
source("do/s_figs/s_fig14_lrts_vs_bvs.R")
source("do/s_figs/s_fig15_water_contact_add_snail_model.R")
source("do/s_figs/s_fig16_water_contact_add_exp_model.R")

#Run s_tabs
source("do/s_tabs/s_tab1_cand_vars.R")
source("do/s_tabs/s_tab2_participant_chars.R")
source("do/s_tabs/s_tab3_water_contact_prop.R")
source("do/s_tabs/s_tab4_water_contact_freq.R")
source("do/s_tabs/s_tab5_water_contact_dura.R")
source("do/s_tabs/s_tab6_water_contact_time_day.R")
source("do/s_tabs/s_tab7_var_sel_lrt_bas.R")

