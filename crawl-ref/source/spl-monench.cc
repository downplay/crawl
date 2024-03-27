/**
 * @file
 * @brief Monster-affecting enchantment spells.
 *           Other targeted enchantments are handled in spl-zap.cc.
**/

#include "AppHdr.h"

#include "spl-monench.h"

#include "coordit.h"
#include "english.h" // apostrophise
#include "env.h"
#include "losglobal.h"
#include "message.h"
#include "spl-util.h"
#include "stringutil.h" // make_stringf
#include "terrain.h"
#include "target.h" // moondial targetter
#include "religion.h"
#include "act-iter.h"
#include "fight.h"
#include "view.h"
#include "colour.h"

int englaciate(coord_def where, int pow, actor *agent)
{
    actor *victim = actor_at(where);

    if (!victim || victim == agent)
        return 0;

    if (agent->is_monster() && mons_aligned(agent, victim))
        return 0; // don't let monsters hit friendlies

    monster* mons = victim->as_monster();

    if (victim->res_cold() > 0
        || victim->is_stationary())
    {
        if (!mons)
            canned_msg(MSG_YOU_UNAFFECTED);
        else if (!mons_is_firewood(*mons))
            simple_monster_message(*mons, " is unaffected.");
        return 0;
    }

    int duration = div_rand_round(roll_dice(3, 1 + pow), 6)
                    - div_rand_round(victim->get_hit_dice() - 1, 2);

    if (duration <= 0)
    {
        if (!mons)
            canned_msg(MSG_YOU_RESIST);
        else
            simple_monster_message(*mons, " resists.");
        return 0;
    }

    if ((!mons && you.get_mutation_level(MUT_COLD_BLOODED))
        || (mons && mons_class_flag(mons->type, M_COLD_BLOOD)))
    {
        duration *= 2;
    }

    // Guarantee a minimum duration if not fully resisted.
    duration = max(duration, 2 + random2(4));

    if (!mons)
        return slow_player(duration);

    return do_slow_monster(*mons, agent, duration * BASELINE_DELAY);
}

spret cast_englaciation(int pow, bool fail)
{
    fail_check();
    mpr("You radiate an aura of cold.");
    apply_area_visible([pow] (coord_def where) {
        return englaciate(where, pow, &you);
    }, you.pos());
    return spret::success;
}

/** Corona a monster.
 *
 *  @param mons the monster to get a backlight.
 *  @returns true if it got backlit (even if it was already).
 */
bool backlight_monster(monster* mons)
{
    const mon_enchant bklt = mons->get_ench(ENCH_CORONA);
    const mon_enchant zin_bklt = mons->get_ench(ENCH_SILVER_CORONA);
    const int lvl = bklt.degree + zin_bklt.degree;

    mons->add_ench(mon_enchant(ENCH_CORONA, 1));

    if (lvl == 0)
        simple_monster_message(*mons, " is outlined in light.");
    else if (lvl == 4)
        simple_monster_message(*mons, " glows brighter for a moment.");
    else
        simple_monster_message(*mons, " glows brighter.");

    return true;
}

bool do_slow_monster(monster& mon, const actor* agent, int dur)
{
    if (mon.stasis())
        return true;

    if (!mon.is_stationary()
        && mon.add_ench(mon_enchant(ENCH_SLOW, 0, agent, dur)))
    {
        if (!mon.paralysed() && !mon.petrified()
            && simple_monster_message(mon, " seems to slow down."))
        {
            return true;
        }
    }

    return false;
}

bool enfeeble_monster(monster &mon, int pow)
{
    const int res_margin = mon.check_willpower(&you, pow);
    vector<enchant_type> hexes;

    if (mons_has_attacks(mon))
        hexes.push_back(ENCH_WEAK);
    if (mon.antimagic_susceptible())
        hexes.push_back(ENCH_ANTIMAGIC);
    if (res_margin <= 0)
    {
        if (mons_can_be_blinded(mon.type))
            hexes.push_back(ENCH_BLIND);
        hexes.push_back(ENCH_DAZED);
    }

    // Resisted the upgraded effects, and immune to the irresistible effects.
    if (hexes.empty())
    {
        return simple_monster_message(mon,
                   mon.resist_margin_phrase(res_margin).c_str());
    }

    const int max_extra_dur = div_rand_round(pow, 40);
    const int dur = 5 + random2avg(max_extra_dur, 3);

    for (auto hex : hexes)
    {
        if (mon.has_ench(hex))
        {
            mon_enchant ench = mon.get_ench(hex);
            ench.duration = max(dur * BASELINE_DELAY, ench.duration);
            mon.update_ench(ench);
        }
        else
            mon.add_ench(mon_enchant(hex, 0, &you, dur * BASELINE_DELAY));
    }

    if (res_margin > 0)
        simple_monster_message(mon, " partially resists.");

    return simple_monster_message(mon, " is enfeebled!");
}

spret cast_vile_clutch(int pow, bolt &beam, bool fail)
{
    spret result = zapping(ZAP_VILE_CLUTCH, pow, beam, true, nullptr, fail);

    if (result == spret::success)
        you.props[VILE_CLUTCH_POWER_KEY].get_int() = pow;

    return result;
}

bool start_ranged_constriction(actor& caster, actor& target, int duration,
                               constrict_type type)
{
    if (!caster.can_constrict(target, type))
        return false;

    if (target.is_player())
    {
        if (type == CONSTRICT_ROOTS)
        {
            you.increase_duration(DUR_GRASPING_ROOTS, duration);
            mprf(MSGCH_WARN, "The grasping roots grab you!");
        }
        else if (type == CONSTRICT_BVC)
        {
            you.increase_duration(DUR_VILE_CLUTCH, duration);
            mprf(MSGCH_WARN, "Zombie hands grab you from below!");
        }
        caster.start_constricting(you);
    }
    else
    {
        enchant_type etype = (type == CONSTRICT_ROOTS ? ENCH_GRASPING_ROOTS
                                                      : ENCH_VILE_CLUTCH);
        auto ench = mon_enchant(etype, 0, &caster, duration * BASELINE_DELAY);
        target.as_monster()->add_ench(ench);
    }

    return true;
}

string moon_phase_name(int phase)
{
    switch (phase)
    {
        case 0:
            return "new";
        case 1:
            return "crescent";
        case 2:
            return "half";
        case 3:
            return "gibbous";
        case 4:
            return "full";
        default:
            return "buggy (" + std::to_string(phase) + ")";
    }
}

static bool _lunar_can_affect(const actor *agent, const monster *target, bool quiet = true)
{
    return !mons_is_conjured(target->type)
            // One very specific exception to moonlight
            && target->type != MONS_MOON_TROLL
            // Monsters can skip affecting friendlies (including themselves)
            && !(agent->is_monster() && mons_aligned(agent, target))
            // Deity protection for allies
            && !god_protects(agent, *target, quiet);
}

static vector<actor *> _get_lunar_fissure_targets(const actor *agent)
{
    // The spell changes player LOS and *then* blinds/damages targets; so the
    // player never knows until their sight returns *which* targets were
    // killed, they will only see experience messages, and a gruesome aftermath.
    vector<actor *> targets;
    if (!agent)
        return targets;

    for (actor_near_iterator ai(agent->pos(), LOS_NO_TRANS);
         ai; ++ai)
    {
        // Player doesn't include themselves in targets (they are
        // automatically affected)
        if (agent->is_player() && ai->is_player())
            continue;
        // And otherwise everything including the player is a valid
        // target (if they can be affected)
        if (ai->is_player()
            || _lunar_can_affect(agent, ai->as_monster(),
                                 !agent->is_player()))
        {
            targets.push_back(*ai);
        }
    }
    return targets;
}

/*
 * Attempt to blast everything in line of sight of the caster with moonlight,
 * blinding and causing radiation damage.
 *
 * @param caster       The caster.
 * @param pow          Power that the spell was cast at.
 * @param min_duration Minimum length of blindness. This will be equal to how
 *                     long the caster has blinded themselves for. Targets will
 *                     be blinded for at least this long or longer.
 */
static void _lunar_fissure_effect(actor* agent, int pow, int min_duration,
                                  vector<actor *> targets, int phase)
{
    // TODO: Would be nice to make some sparks and some moonlight sparkles (from runelight)
    for (actor* target : targets)
    {
        if (target->is_player())
        {
            // How many eyes does the player have?
            const bool one_eye = you.has_mutation(MUT_MISSING_EYE)
                                 && !you.has_mutation(MUT_EYEBALLS);
            mprf("Your eye%s, accustomed only to darkness, %s filled with moonbeams.",
                 one_eye ? "" : "s", conjugate_verb("are", !one_eye).c_str());
            blind_player(min_duration
                           + (agent->is_player() ? 0 : random2(div_rand_round(pow, 50))),
                         ETC_MOON);
            // Bonus contamination for player undead (to punish them for avoiding
            // malmutations)
            const double undead_mult = you.is_lifeless_undead(true) ? 1.5 : 1;
            // At lowest power 500-1000 rads, at max power 1500-5000 rads
            // (pre undead multiplier)
            contaminate_player(random_range(500 + pow * 5, 1000 + pow * 20)
                               * undead_mult, agent->is_player());
        }
        else
        {
            // Monster save roll
            ASSERT(target->is_monster());
            monster *mons = target->as_monster();
            if (!x_chance_in_y(pow, 200))
            {
                simple_monster_message(*mons, " is unaffected.");
                continue;
            }

            const int duration = min_duration + random2(div_rand_round(pow, 30));
            auto ench = mon_enchant(ENCH_BLIND, 1, agent, BASELINE_DELAY * duration);

            // TODO: Will fail to blind monsters flagged as unblindable but this should be an
            // exception. So I think we need to create a separate enchantment.
            if (mons->add_ench(ench) && you.can_see(*target))
            {
                simple_monster_message(*mons, " is outlined by radiant moonbeams.");
            }
            // If they were unblinded then definitely radiate; otherwise
            // chance also increases with phase.
            if (!mons_can_be_blinded(mons->type) || x_chance_in_y(2 + phase, 8))
                mons->malmutate("moonlight");
        }
    }
}

/**
 * Attempt to cast the spell "Lunar Fissure", blinding damaging & deforming everything
 * in LOS as well as reducing player LOS to zero.
 *
 * @param powc   The power at which the spell is being cast.
 * @param caster The actor casting the spell.
 * @param fail   Whether the player has failed to cast the spell.
 * @return       spret::abort if the player changed their mind about casting after
 *               realizing they would hit an ally; spret::fail if they failed the
 *               cast chance; spret::success otherwise.
 */
spret cast_lunar_fissure(int powc, actor &caster, bool fail)
{
    const int phase = (powc - 1) / 50 + 1;
    
    auto affected = [&caster](const actor *act) -> bool
    {
        return !act->is_player() && _lunar_can_affect(&caster, act->as_monster());
    };

    targeter_radius hitfunc(&caster, LOS_NO_TRANS);

    if (caster.is_player())
    {
        // "Moonlight" is already a verb, although it means something different
        if (stop_attack_prompt(hitfunc, "moonlight", affected))
            return spret::abort;
    }

    fail_check();

    if (caster.is_player())
        mpr("Everything is bathed in purest moonlight!");
    else
        simple_monster_message(*caster.as_monster(),
                               " bathes everything in moonlight!");

    flash_view_delay(UA_PLAYER, LIGHTCYAN, 300, &hitfunc);

    // Who can we hit?
    vector<actor *> targets = _get_lunar_fissure_targets(&caster);

    // Select either 25%, 50%, 75%, or 100% of targets,
    // depending on the "phase" of the moon
    const size_t num_to_pick = div_round_up(targets.size() * phase, 4);
    unordered_set<int> chosen_indices;
    vector<actor *> chosen_targets;
#ifdef DEBUG
    mprf("Moon: Pow %d Phase %d Picking %ld targets (chance %d%%)",
          powc, phase, num_to_pick, phase * 25);
#endif
    while (chosen_targets.size() < num_to_pick)
    {
        int iter = 0;
        while (iter < 100)
        {
            iter++;
            const int index = random2(targets.size());
            if (chosen_indices.count(index) == 0)
            {
                chosen_indices.insert(index);
                chosen_targets.push_back(targets[index]);
                break;
            }
        }
    }

    // Always target player when they are casting
    if (caster.is_player())
        chosen_targets.push_back(&you);

    // Up to 10 turns at max power
    const int blind_duration = 5 + div_rand_round(powc, 50) + div_rand_round(random2(powc), 20);

    // Run the effect on all targets
    _lunar_fissure_effect(&caster, powc, blind_duration,
                          chosen_targets, phase);

    return spret::success;
}
