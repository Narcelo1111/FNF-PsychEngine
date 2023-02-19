local phase2 = false
local phase1 = false
function onCreate()
	phase1 = songName == 'The Idea' or songName == 'The Idea Minus'
	phase2 = songName == 'Combo Meal' or songName == 'Combo Meal Erect'
	--main thing
	makeAnimatedLuaSprite('theWall','ronald/wall', -80, 0);
	addAnimationByPrefix('theWall','bop','Símbolo 12 instancia 1',24,false)
	
	scaleObject('theWall', 2.94, 2.94);

	makeAnimatedLuaSprite('inuko','ronald/inuko-boper', 330, 740);
	addAnimationByPrefix('inuko','bop','xo window',24,false)
	scaleObject('inuko', 2.94, 2.94);

	addLuaSprite('inuko',false);
	addLuaSprite('theWall',false);

	--makeLuaSprite('theGround','ronald/floor',-0 , 1470);
	--addLuaSprite('theGround',false);
	--scaleObject('theGround', 2, 2);

	if phase1 and not lowQuality then
		makeAnimatedLuaSprite('theBackBops','ronald/phase1/back-bopers', 710, 850);
	    addAnimationByPrefix('theBackBops','bop','mcworkers',24,false)
	    addLuaSprite('theBackBops',false);
	    scaleObject('theBackBops', 3.6, 3.6);

	    makeAnimatedLuaSprite('theMidBops','ronald/phase1/mid-bopers', 380, 770);
 	    addAnimationByPrefix('theMidBops','bop','peopleback',24,false)
	    addLuaSprite('theMidBops',false);
	    scaleObject('theMidBops', 4.6, 4.6);
	    setLuaSpriteScrollFactor('theMidBops',0.98 , 0.98);

	    makeAnimatedLuaSprite('theFrontBops','ronald/phase1/front-bopers', -450, 910);
	    addAnimationByPrefix('theFrontBops','bop','peoplefront',24,false)
	    addLuaSprite('theFrontBops',true);
	    scaleObject('theFrontBops', 5.2, 5.2);
		setLuaSpriteScrollFactor('theFrontBops',0.84 , 0.84);

		makeAnimatedLuaSprite('theFrontBops2','ronald/phase1/front-bopers', -1450, 910); --theres actually 2 lol, because the gap was too big in between
	    addAnimationByPrefix('theFrontBops2','bop','peoplefront',24,false)
	    addLuaSprite('theFrontBops2',true);
	    scaleObject('theFrontBops2', 5.2, 5.2);
		setLuaSpriteScrollFactor('theFrontBops2',0.84 , 0.84);

	end

	
	

	if phase2 and not lowQuality then
		makeAnimatedLuaSprite('theBackBops','ronald/2phase/back-bopers', 710, 810);
	    addAnimationByPrefix('theBackBops','bop','mcworkers',24,false)
	    addLuaSprite('theBackBops',false);
	    scaleObject('theBackBops', 3.6, 3.6);

	    makeAnimatedLuaSprite('theMidBops','ronald/2phase/mid-bopers', 200, 820);
 	    addAnimationByPrefix('theMidBops','bop','peopleback',24,false)
	    addLuaSprite('theMidBops',false);
	    scaleObject('theMidBops', 4, 4);
	    setLuaSpriteScrollFactor('theMidBops',0.98 , 0.98);

	    makeAnimatedLuaSprite('theFrontBops','ronald/2phase/front-bopers', 1850, 1000);
	    addAnimationByPrefix('theFrontBops','bop','peoplefront',24,false)
	    addLuaSprite('theFrontBops',true);
	    scaleObject('theFrontBops', 5.2, 5.2);
	    setLuaSpriteScrollFactor('theFrontBops',0.84 , 0.84);

		makeAnimatedLuaSprite('shag','ronald/2phase/shaggy-boper', -230, 970);
	    addAnimationByPrefix('shag','bop','shaggy',24,false)
	    addLuaSprite('shag',true);
	    scaleObject('shag', 5.2, 5.2);
	    setLuaSpriteScrollFactor('shag',0.84 , 0.84);

		addHaxeLibrary('ColorSwap')
		runHaxeCode([[
			var colorSwap = new ColorSwap();
			var colorSwapDarker = new ColorSwap();
			game.variables['desaturation'] = colorSwap;
			game.variables['desaturationDarker'] = colorSwapDarker;
			game.modchartSprites.get("theMidBops").shader = colorSwapDarker.shader;
			game.modchartSprites.get("theFrontBops").shader = colorSwap.shader;
			game.modchartSprites.get("theBackBops").shader = colorSwapDarker.shader;
			game.modchartSprites.get("theWall").shader = colorSwap.shader;
			//colorSwap.saturation = -0.45;
			//colorSwap.brightness = -0.4;
			//colorSwapDarker.saturation = -0.45;
			//colorSwapDarker.brightness = -0.47;
		]])


		--[[makeAnimatedLuaSprite('theWallFrozen','ronald/2phase/frozen/wall', 0, 0);
		addAnimationByPrefix('theWallFrozen','bop','Símbolo 12 instancia 1',24,false)
		addLuaSprite('theWallFrozen',false);
		scaleObject('theWallFrozen', 1, 1);
	
		makeLuaSprite('theGroundFrozen','ronald/2phase/frozen/floor',0 , 0);
		addLuaSprite('theGroundFrozen',false);
		scaleObject('theGroundFrozen', 1, 1);
	
		makeAnimatedLuaSprite('theBackBopsFrozen','ronald/2phase/frozen/back-bopers', 710, 848);
		addAnimationByPrefix('theBackBopsFrozen','bop','Símbolo 15 instancia 1',24,false)
		addLuaSprite('theBackBopsFrozen',false);
		scaleObject('theBackBopsFrozen', 1, 1);
	
		makeAnimatedLuaSprite('theMidBopsFrozen','ronald/2phase/frozen/mid-bopers', 100, 800);
		addAnimationByPrefix('theMidBopsFrozen','bop','peopleback',24,false)
		addLuaSprite('theMidBopsFrozen',false);
		scaleObject('theMidBopsFrozen', 1, 1);
		setLuaSpriteScrollFactor('theMidBopsFrozen',0.98 , 0.98);
	
		makeAnimatedLuaSprite('theFrontBopsFrozen','ronald/2phase/frozen/front-bopers', -310, 950);
		addAnimationByPrefix('theFrontBopsFrozen','bop','peoplefront instancia 1',24,false)
		addLuaSprite('theFrontBopsFrozen',true);
		scaleObject('theFrontBopsFrozen', 1, 1);
		setLuaSpriteScrollFactor('theFrontBopsFrozen',0.84 , 0.84);
	
		makeLuaSprite('thePostersFrozen','ronald/2phase/frozen/posters',-400 , 200);
		addLuaSprite('thePostersFrozen',false);
		scaleObject('thePostersFrozen', 1, 1);
	
		setProperty('theWallFrozen.alpha', 0)
		setProperty('theGroundFrozen.alpha', 0)
		setProperty('theBackBopsFrozen.alpha', 0)
		setProperty('theMidBopsFrozen.alpha', 0)
		setProperty('theFrontBopsFrozen.alpha', 0)
		setProperty('thePostersFrozen.alpha', 0)]]--
	end

	--makeLuaSprite('thePosters','ronald/posters',-400 , 200);
	--addLuaSprite('thePosters',false);
	--scaleObject('thePosters', 2, 2);



	

end
function onCreatePost()
	--setProperty('gf.visible', false)
end

function onStepHit()
	--setProperty('camGame.zoom', 0.2)
	if curStep % 8 == 4 then
		objectPlayAnimation('inuko', 'bop', true)

		if (phase2 or phase1) and not lowQuality then 
			if not frozen then 
				objectPlayAnimation('theMidBops', 'bop', true)
				objectPlayAnimation('theFrontBops', 'bop', true)
				objectPlayAnimation('theBackBops', 'bop', true)
				if phase1 then 
					objectPlayAnimation('theFrontBops2', 'bop', true)
				end
			end
			objectPlayAnimation('shag', 'bop', true)
		end
	end
end


local frozen = false
function onEvent(tag, val1, val2)
	if tag == 'Toggle BG Freeze' then 
		frozen = not frozen
		if frozen then
			if not lowQuality then 
				setProperty('theBackBops.animation.curAnim.frameRate', 0)
				setProperty('theMidBops.animation.curAnim.frameRate', 0)
				setProperty('theFrontBops.animation.curAnim.frameRate', 0)
				
			end
			runHaxeCode([[
				game.variables['desaturation'].saturation = -0.45;
				game.variables['desaturation'].brightness = -0.4;
				game.variables['desaturationDarker'].saturation = -0.45;
				game.variables['desaturationDarker'].brightness = -0.49;
			]])


			characterPlayAnim('gf', 'chasam', true)
			setProperty('gf.specialAnim', true)
		else
			if not lowQuality then 
			--setProperty('defaultCamZoom', 0.62)
			setProperty('theBackBops.animation.curAnim.frameRate', 24)
			setProperty('theMidBops.animation.curAnim.frameRate', 24)
			setProperty('theFrontBops.animation.curAnim.frameRate', 24)
			end
			runHaxeCode([[
				game.variables['desaturation'].saturation = 0;
				game.variables['desaturation'].brightness = 0;
				game.variables['desaturationDarker'].saturation = 0;
				game.variables['desaturationDarker'].brightness = 0;
			]])

			
			setProperty('gf.specialAnim', false)
			characterDance('gf')
		end
	end
end