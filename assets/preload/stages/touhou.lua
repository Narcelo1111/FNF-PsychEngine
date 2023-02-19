local dadx,dady,bfx,bfy,xox,xoy = 0
local floatxo = false
function onCreatePost()
    makeLuaSprite('bg','touhou/touhou_bg', 0, 0);
    addLuaSprite('bg',false);
    setGraphicSize('bg', 1280/0.4)
    setScrollFactor('bg', 0.3,0.3)
    
    screenCenter('bg', 'xy')
    --makeLuaSprite('bg2','touhou/touhou_bg', 0, 0);
    setProperty('bg.x', getProperty('bg.x')-100)
    --addLuaSprite('bg2',false);
    --setGraphicSize('bg2', 1280/0.4)
    --setScrollFactor('bg2', 0.5,0.5)

    setScrollFactor('dad', 0.75, 0.75)
    
    --setProperty('bg2.x', getProperty('bg2.x')-getProperty('bg2.width'))
    dadx = getProperty('dad.x')
    dady = getProperty('dad.y')
    bfx = getProperty('boyfriend.x')
    bfy = getProperty('boyfriend.y')
    xox = getProperty('gf.x')
    xoy = getProperty('gf.y')
    setProperty('gf.y', getProperty('gf.y')+1200)
end

function onUpdate(elapsed)
    setProperty('dad.x', dadx + math.cos(getSongPosition()*0.001)*30)
    setProperty('dad.y', dady + math.sin(getSongPosition()*0.001*1.5)*50)

    setProperty('boyfriend.x', bfx + math.sin(getSongPosition()*0.001)*15)
    setProperty('boyfriend.y', bfy + math.cos(getSongPosition()*0.001*1.5)*30)
    if floatxo then 
        setProperty('gf.x', xox - math.sin(getSongPosition()*0.001)*15)
        setProperty('gf.y', xoy - math.cos(getSongPosition()*0.001*1.5)*30)
    end

end

function onStepHit()--stupid shit ig

    if curStep == 768+4 then 
        floatxo = true
    end
end