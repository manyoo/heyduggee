module Main where

import Prelude

import Data.Array (findIndex, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Node.Express.App (get, listenHttp)
import Node.Express.Handler (HandlerM)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (send)

main :: Effect Unit
main = void $ listenHttp app 8033 callback
    where app = get "/heyduggee/:name" handleReq
          callback _ = pure unit

handleReq :: HandlerM Unit
handleReq = do
    p <- getRouteParam "name"
    case p of
        Just name -> send $ fromMaybe ("抱歉，没找到" <> name) $ toString <$> searchName name
        Nothing   -> send "好可惜，没找到哦"

type Season = Int
type Episode = Int

toString :: Tuple Season Episode -> String
toString (Tuple s e) = "第" <> show s <> "季第" <> show e <> "集，合集第" <> show totalIdx <> "集"
    where totalIdx = case s of
              1 -> e
              2 -> 51 + e
              3 -> 51 + 52 + e
              _ -> e

searchName :: String -> Maybe (Tuple Season Episode)
searchName p = snd $ foldl f (Tuple 1 Nothing) [firstSeason, secondSeason, thirdSeason]
    where f (Tuple idx Nothing) arr = Tuple (idx + 1) (Tuple idx <$> indexOf p arr)
          f v@(Tuple idx (Just _)) arr = v

indexOf :: String -> Array String -> Maybe Int
indexOf p d = (+) 1 <$> findIndex (contains $ Pattern p) d

firstSeason :: Array String
firstSeason = [
    "1.阿奇和画画徽章",
    "2.阿奇和蛋糕徽章",
    "3.阿奇和头发徽章",
    "4.阿奇和暑假徽章",
    "5.阿奇和救援徽章",
    "6.阿奇和超级小朋友徽章",
    "7.阿奇和果酱徽章",
    "8.阿奇和寻宝徽章",
    "9.阿奇和稻草人徽章",
    "10.阿奇和扮鬼脸徽章",
    "11.阿奇和跳跃徽章",
    "12.阿奇和叶子徽章",
    "13.阿奇和蛋饼徽章",
    "14.阿奇和种食物徽章",
    "15.阿奇和充气泳池徽章",
    "16.阿奇和纸船徽章",
    "17.阿奇和城堡徽章",
    "18.阿奇和展示介绍徽章",
    "19.阿奇和木马徽章",
    "20.阿奇和早日康复徽章",
    "21.阿奇和打嗝徽章",
    "22.阿奇和迷宫徽章",
    "23.阿奇和祈雨舞徽章",
    "24.阿奇和橡果徽章",
    "25.阿奇和气球徽章",
    "26.阿奇和捉迷藏徽章",
    "27.阿奇和绵羊徽章",
    "28.阿奇和蜘蛛徽章",
    "29.阿奇和我们爱动物徽章",
    "30.阿奇和雪人徽章",
    "31.阿奇和装潢徽章",
    "32.阿奇和整洁徽章",
    "33.阿奇和马戏团徽章",
    "34.阿奇和鸡蛋徽章",
    "35.阿奇和小狗徽章",
    "36.阿奇和毛毛虫徽章",
    "37.阿奇和纸箱徽章",
    "38.阿奇和沙堡徽章",
    "39.阿奇和足球徽章",
    "40.阿奇和偶戏徽章",
    "41.阿奇和小鸟徽章",
    "42.阿奇和大游行徽章",
    "43.阿奇和小心徽章",
    "44.阿奇和急救徽章",
    "45.阿奇和泡泡徽章",
    "46.阿奇和潜艇徽章",
    "47.阿奇和侦探徽章",
    "48.阿奇和裁缝徽章",
    "49.阿奇和惊喜徽章",
    "50.阿奇和泰迪熊徽章",
    "51.阿奇和故事徽章"
    ]

secondSeason :: Array String
secondSeason = [
    "1.阿奇和制作音乐徽章",
    "2.阿奇和吹口哨徽章",
    "3.阿奇和形状徽章",
    "4.阿奇和果汁徽章",
    "5.阿奇和脚印徽章",
    "6.阿奇和化石徽章",
    "7.阿奇和树屋徽章",
    "8.阿奇和风筝徽章",
    "9.阿奇和蝌蚪徽章",
    "10.阿奇和蜂蜜徽章",
    "11.阿奇和陶艺徽章",
    "12.阿奇和合作徽章",
    "13.阿奇和跳舞虫徽章",
    "14.阿奇和火车徽章",
    "15.阿奇和披萨徽章",
    "16.阿奇和治好病徽章",
    "17.阿奇和露营徽章",
    "18.阿奇和交朋友徽章",
    "19.阿奇和游河徽章",
    "20.阿奇和瑜伽徽章",
    "21.阿奇和搜集徽章",
    "22.阿奇和影子徽章",
    "23.阿奇和戏剧徽章",
    "24.阿奇和好记性徽章",
    "25.阿奇和慢慢来徽章",
    "26.阿奇和趣味障碍徽章",
    "27.阿奇和帕帕徽章",
    "28.阿奇和太空徽章",
    "29.阿奇和大丰收徽章",
    "30.阿奇和色彩徽章",
    "31.阿奇和树枝徽章",
    "32.阿奇和声音徽章",
    "33.阿奇和荒岛徽章",
    "34.阿奇和睡觉徽章",
    "35.阿奇和勇敢小香蕉徽章",
    "36.阿奇和自来水徽章",
    "37.阿奇和交通徽章",
    "38.阿奇和扮装徽章",
    "39.阿奇和好相处徽章",
    "40.阿奇和婚礼徽章",
    "41.阿奇和爷爷奶奶徽章",
    "42.阿奇和整理排好徽章",
    "43.阿奇和好好照顾徽章",
    "44.阿奇和好好笑徽章",
    "45.阿奇和歌唱徽章",
    "46.阿奇和玩乐徽章",
    "47.阿奇和派对徽章",
    "48.阿奇和导游徽章",
    "49.阿奇和航海徽章",
    "50.阿奇和钥匙徽章",
    "51.阿奇和时装徽章",
    "52.阿奇和眼镜徽章"
    ]

thirdSeason :: Array String
thirdSeason = [
    "1.阿奇和好好刷牙徽章",
    "2.阿奇和鸭鸭徽章",
    "3.阿奇和放假徽章",
    "4.阿奇和伪装隐身徽章",
    "5.阿奇和进屋里徽章",
    "6.阿奇和安静徽章",
    "7.阿奇和笔友徽章",
    "8.阿奇和去郊游徽章",
    "9.阿奇和树徽章",
    "10.阿奇和起司徽章",
    "11.阿奇和电台徽章",
    "12.阿奇和相反徽章",
    "13.阿奇和早餐徽章",
    "14.阿奇和全家福合照徽章",
    "15.阿奇和未来徽章",
    "16.阿奇和生活哲学徽章",
    "17.阿奇和分享徽章",
    "18.阿奇和历史徽章",
    "19.阿奇和艺术徽章",
    "20.阿奇和疯狂高球徽章",
    "21.阿奇和大奇案徽章",
    "22.阿奇和选举徽章",
    "23.阿奇和混音带徽章",
    "24.阿奇和味觉徽章",
    "25.阿奇和生物学徽章",
    "26.阿奇和水滩徽章",
    "27.阿奇和游戏节目徽章",
    "28.阿奇和牛仔徽章",
    "29.阿奇和我的最爱徽章",
    "30.阿奇和绿雕塑徽章",
    "31.阿奇和阿卡贝拉徽章"
    ]
