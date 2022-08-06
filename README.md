# Covid Monitor Mode ![wftpl](http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl-badge-4.png)

![Preview](screenshot.png)

此插件依赖 `curl`，所以确保系统中 `curl` 命令可以正常使用

`covid-monitor-area-list` 用于配置省份

`covid-monitor-update-interval` 用于配置数据更新的间隔时间（单位秒，默认600）

配置示例:

```
(use-package covid-monitor-mode
  :config
  (setq covid-monitor-area-list '("浙江"))
  (covid-monitor-mode))
```
