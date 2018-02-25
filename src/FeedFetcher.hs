{-# LANGUAGE OverloadedStrings #-}

module FeedFetcher (
  feedFetcher
) where

import qualified Data.Text as T

import Control.Concurrent   (threadDelay)
import System.Random        (randomRIO)
import Database.Persist.Sql (ConnectionPool, insert, runSqlPool)
import Control.Monad        (void)

import Model

feedFetcher :: ConnectionPool -> IO ()
feedFetcher sqlConnPool = do
  threadDelay 300000000 -- wait five minutes
  i <- randomRIO (0, length links - 1)
  let action = insert $ Deviation (links !! i) "title" "author"
  void $ runSqlPool action sqlConnPool
  feedFetcher sqlConnPool

links :: [T.Text]
links =
  [ "https://pre00.deviantart.net/9d39/th/pre/f/2018/046/8/a/princess_and_cannibal_by_irenetall-dc393kz.jpg"
  , "https://pre00.deviantart.net/dd6a/th/pre/i/2018/007/2/7/bust_sketch_s_by_katherine_olenic-dbz8fjy.jpg"
  , "https://pre00.deviantart.net/064d/th/pre/i/2018/019/4/f/wedding_by_katherine_olenic-dc0ezwh.jpg"
  , "https://pre00.deviantart.net/92e6/th/pre/i/2018/041/4/1/__drowning_in_despair___by_viralnekovic-dc2sob7.jpg"
  , "https://pre00.deviantart.net/047b/th/pre/f/2018/032/c/9/legacy_by_tobiasroetsch-dc1tzhu.jpg"
  , "https://img00.deviantart.net/bfcf/i/2017/073/e/0/inspiration_by_beckykidus-db29mnc.jpg"
  , "https://pre00.deviantart.net/b1ed/th/pre/f/2016/284/8/d/ink10__vanessa__2__by_gossart1323-dakpolf.jpg"
  , "https://orig00.deviantart.net/20e4/f/2018/028/2/7/little_treasure_by_darknatasha-dc1fixe.jpg"
  , "https://img00.deviantart.net/71a6/i/2018/029/e/d/copic_airbrush_test_drawing_by_scribblefix-dc1jbjn.jpg"
  , "https://img00.deviantart.net/25aa/i/2017/288/e/8/love_is_fierce_by_lexidus-dbqmnwz.jpg"
  , "https://img00.deviantart.net/58ce/i/2017/299/9/4/the_blind_gallion_by_lexidus-dbrud6g.jpg"
  , "https://pre00.deviantart.net/1196/th/pre/i/2018/023/9/c/luke_by_blackbirdink-dc0yn4p.png"
  , "https://pre00.deviantart.net/f89f/th/pre/i/2018/026/8/b/will_o__the_wisp_by_kate_fox-dc18nb5.jpg"
  , "https://pre00.deviantart.net/1e9f/th/pre/f/2018/015/3/6/crane_tattoo_by_m_u_h_a-dc03vbq.png"
  , "https://pre00.deviantart.net/269d/th/pre/f/2018/013/9/3/mlpfim__two_best_daughters_by_dsp2003-dbzsdia.png"
  , "https://pre00.deviantart.net/f929/th/pre/i/2018/008/b/a/stairs_to_the_fox_temple_by_kate_fox-dbzdpsa.jpg"
  , "https://orig00.deviantart.net/edfb/f/2018/010/1/f/mini_sketch__azale_by_blackbirdink-dbzjjl0.png"
  , "https://orig00.deviantart.net/ee53/f/2018/004/3/5/neurology_congress_by_m_u_h_a-dbyxso6.jpg"
  , "https://img00.deviantart.net/b0d7/i/2018/002/8/d/orange_1_by_lilsuika-dbyoj22.jpg"
  , "https://pre00.deviantart.net/65da/th/pre/i/2017/355/5/5/a_midwinter_night_s_dream_by_borda-dbxd9za.jpg"
  , "https://pre00.deviantart.net/c108/th/pre/f/2017/355/0/7/femfortress_by_dariiy-dbxed8g.jpg"
  , "https://pre00.deviantart.net/dcf0/th/pre/i/2017/361/2/3/shao_kahn_mk2_by_fear_sas-dbxz50k.jpg"
  , "https://pre00.deviantart.net/4317/th/pre/i/2017/237/e/c/c_i_c_l_i_s_t_a___c_a_l_a_v_e_r_a_by_australatinaztlan-dblazvm.jpg"
  , "https://orig00.deviantart.net/06ae/f/2017/346/f/e/drawing__12__by_xxnightmare0811xx-dbwk5nm.png"
  , "https://pre00.deviantart.net/dcb7/th/pre/i/2017/342/c/c/jelly_screenshot_by_varien__autentix-dbw41jx.png"
  , "https://pre00.deviantart.net/2ff6/th/pre/i/2016/191/8/a/red_crown_crane_taking_off_by_katherine_olenic-da9gjgx.jpg"
  , "https://pre00.deviantart.net/cec1/th/pre/i/2016/355/e/c/mrs_pine_rose_pots_and_a_mint_leaf_by_katherine_olenic-dase15h.jpg"
  , "https://pre00.deviantart.net/cbc9/th/pre/i/2017/296/6/2/wonderlicious_by_katherine_olenic-dbrh1gj.png"
  , "https://pre00.deviantart.net/c410/th/pre/i/2017/337/f/a/trytalik_by_varien__autentix-dbvmpzs.jpg"
  , "https://orig00.deviantart.net/59a4/f/2017/330/0/8/cosmos_by_hideyoshi-dbuwo2s.jpg"
  , "https://img00.deviantart.net/4933/i/2017/316/b/d/some_like_it_hot_by_katherine_olenic-dbtkbus.png"
  , "https://pre00.deviantart.net/6464/th/pre/f/2017/329/6/1/cafej_by_rampoz-dbuu47n.jpg"
  , "https://pre00.deviantart.net/5c67/th/pre/i/2017/326/5/1/hyper_heroes_fan_art___tictac_tom___by_fear_sas-dbui79z.jpg"
  , "https://img00.deviantart.net/0614/i/2018/035/b/e/autumn_dancing_in_the_breeze_by_buffalodog-dbtmzjf.jpg"
  , "https://pre00.deviantart.net/a2c6/th/pre/i/2017/314/3/7/my_dia_tonight_by_natnatko-dbtd4w0.png"
  , "https://pre00.deviantart.net/4822/th/pre/f/2017/322/b/0/maintenance_routine_by_varien__autentix-dbu3op1.jpg"
  , "https://pre00.deviantart.net/3a4f/th/pre/i/2017/317/6/3/censorship_gul_dan_by_chamucaselamor69-dbto5sq.jpg"
  , "https://pre00.deviantart.net/09e7/th/pre/i/2017/313/5/8/cyberorc_by_chamucaselamor69-dbta6j9.jpg"
  , "https://pre00.deviantart.net/d5c0/th/pre/f/2017/302/8/5/bookbinder_s_daughter_and_shaman_s_son_by_megatruh-dbs2us8.jpg"
  , "https://pre00.deviantart.net/d9dc/th/pre/f/2017/295/0/1/ellie_mansion_by_arsenixc-dbrgcbp.jpg"
  , "https://pre00.deviantart.net/8fc8/th/pre/i/2017/296/6/0/speedpaint__103_by_sylar113-dbriybn.jpg"
  , "https://orig00.deviantart.net/eea5/f/2017/299/e/2/stasis_by_alexandreev-dbrsetk.jpg"
  , "https://pre00.deviantart.net/469c/th/pre/i/2016/361/2/1/captain_xeves_foss_by_varien__autentix-dasy6gc.jpg"
  , "https://orig00.deviantart.net/94d0/f/2017/288/5/2/illustration_for_one_unfinished_project_by_alexandreev-dbqcmqe.jpg"
  , "https://orig00.deviantart.net/4759/f/2017/279/3/7/japan_birdgirl_by_m_u_h_a-dbpqo7i.jpg"
  , "https://img00.deviantart.net/7c7e/i/2017/290/5/f/st_clarke_valkyrie_by_chamucaselamor69-dbqxuz1.jpg"
  , "https://img00.deviantart.net/6a3c/i/2017/269/3/6/speedpaint__97_by_sylar113-dbon9hx.jpg"
  , "https://img00.deviantart.net/5dcf/i/2017/269/6/b/speedpaint__99_by_sylar113-dbon9vi.jpg"
  , "https://pre00.deviantart.net/4d0c/th/pre/f/2017/269/e/3/warm_by_thepingdelf-dbonxbk.jpg"
  , "https://pre00.deviantart.net/8373/th/pre/f/2017/002/d/1/legend_of_galactic_heroes___hyperion_by_hideyoshi-daty7zl.jpg"
  , "https://pre00.deviantart.net/0157/th/pre/f/2017/266/8/f/8f4a5a1295d1c738c9d08798d0ee1456-dboax4r.jpg"
  , "https://pre00.deviantart.net/5c84/th/pre/i/2017/261/3/b/second_attempt_of_digital_painting__by_albabg-dbnsmti.jpg"
  , "https://pre00.deviantart.net/b9a0/th/pre/f/2017/252/e/f/generator_of_ideas_by_rhads-dbmwmqi.jpg"
  , "https://img00.deviantart.net/121e/i/2017/232/5/d/starfire__commission__by_taiss14-dbkr3ad.jpg"
  , "https://pre00.deviantart.net/3596/th/pre/f/2017/233/9/1/the_sun_during_winter_by_rampoz-dbktexn.png"
  , "https://orig00.deviantart.net/3cd2/f/2017/233/2/2/ignition_by_alexandreev-dbkq2ze.jpg"
  , "https://danlev.deviantart.com/journal/DeviantArt-Is-Switching-To-HTTPS-697996906"
  , "https://pre00.deviantart.net/ab8a/th/pre/i/2017/222/e/c/a_plane__by_taiss14-dbjjb03.jpg"
  , "https://pre00.deviantart.net/d230/th/pre/f/2017/189/3/9/39bd69f003c28ca4f242ddf6821df077-dbfl8gu.jpg"
  ]
