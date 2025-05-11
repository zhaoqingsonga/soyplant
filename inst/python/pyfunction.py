# -*- coding: utf-8 -*-
"""
Created on Sun Mar  9 09:36:13 2025

@author: zhaoq
"""

# 安装拼音转换库（若未安装）
# pip install pypinyin

from pypinyin import lazy_pinyin, Style, load_phrases_dict

def chinese_to_pinyin(text_list, capitalize=True, tone=False):
    """
    将中文文本列表转换为拼音
    
    参数：
    text_list: list[str] - 要转换的中文字符串列表
    capitalize: bool - 是否首字母大写 (默认True)
    tone: bool - 是否显示声调 (默认False)
    
    返回：
    list[str] - 转换后的拼音结果列表
    """
    # 配置拼音风格
    style = Style.TONE if tone else Style.NORMAL
    
    # 处理多音字（示例：修正「重庆」的发音）
    load_phrases_dict({
        "重庆": [["chōng"], ["qìng"]]
    })
    
    # 批量转换
    results = []
    for text in text_list:
        # 获取拼音列表
        pinyin_list = lazy_pinyin(
            text,
            style=style,
            neutral_tone_with_five=True
        )
        
        # 格式化处理
        if capitalize:
            formatted = [s.capitalize() for s in pinyin_list]
        else:
            formatted = pinyin_list
        
        # 合并结果
        results.append(''.join(formatted))
    
    return results

