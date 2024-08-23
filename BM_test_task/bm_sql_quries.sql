 -- 1.  Знайти тривалість підписки з заданим <id>

select subscription_duration
from bm_subscriptions
where subscriber_id = :subscriber_id;

 -- 2.  Знайти список підписок для додатку з заданим <id>
select subscription_name,
       subscription_apple_id,
       subscription_duration
from bm_subscriptions
where app_apple_id = :app_apple_id
group by subscription_name, subscription_apple_id, subscription_duration;

-- 3. Визначити який додаток приніс більше доходу за заданий період
-- Поле developer_proceeds має значення у різних валютах,
-- тому спочатку створюється тимчасова таблиця для конвертації у USD за певним курсом,
-- а вже потім відбувається підрахунок доходу

create temporary table currency_rates (
    currency VARCHAR(3),
    rate DECIMAL(10, 4)
);

insert into currency_rates (currency, rate) values
('USD', 1.0000),
('EUR', 1.1800),
('JPY', 0.0091),
('GBP', 1.3800),
('AUD', 0.7700),
('CAD', 0.8000),
('CHF', 1.0900),
('CNY', 0.1550),
('HKD', 0.1290),
('NZD', 0.7200),
('PHP', 0.0200),
('THB', 0.0300),
('IDR', 0.0001),
('TRY', 0.1200),
('RON', 0.2400),
('ILS', 0.3100),
('HRK', 0.1600),
('NOK', 0.1200),
('INR', 0.0135),
('HUF', 0.0033),
('MXN', 0.0500),
('RUB', 0.0140),
('KRW', 0.0009),
('NGN', 0.0024),
('BGN', 0.6000),
('TWD', 0.0360),
('ZAR', 0.0700),
('SAR', 0.2700),
('PLN', 0.2600),
('COP', 0.0003),
('TZS', 0.0004),
('VND', 0.0000),
('KZT', 0.0024),
('MYR', 0.2400),
('PKR', 0.0064),
('SGD', 0.7500),
('SEK', 0.1200),
('QAR', 0.2700),
('CZK', 0.0460),
('PEN', 0.2600),
('BRL', 0.1900),
('DKK', 0.1600),
('AED', 0.2700),
('CLP', 0.0014),
('EGP', 0.0640);

with converted_revenue as (
    select
        app_name,
        app_apple_id,
        round(sum(developer_proceeds * coalesce(cr.rate, 1)),2)
         as usd_revenue
    from bm_subscriptions bs
    left join currency_rates cr on bs.proceeds_currency = cr.currency
    where event_date between '2019-02-01' AND '2029-03-11'
    group by app_name, app_apple_id
)
select app_name, app_apple_id, usd_revenue as total_revenue_usd
from converted_revenue
order by usd_revenue desc;

-- 4. Знайти конверсію з оформлення пробного періоду в
-- успішний платіж для користувачів, що зробили підписку <id> в дату <date>

with trial_subscriptions as (
    select
        subscriber_id
    from bm_subscriptions
    where subscription_apple_id = :subs_apple_id
    and event_date = '2019-02-05'
    and introductory_price_type is not null
),
paid_subscriptions as (
    select subscriber_id
    from bm_subscriptions
    where event_date = '2019-02-05'
    and subscription_apple_id = :subs_apple_id
    and customer_price > 0
    and subscriber_id in (select subscriber_id from trial_subscriptions)
)
select
    count(distinct b.subscriber_id) as paid_subscriptions,
    count(distinct a.subscriber_id) as trial_subscriptions,
    round(
    count(distinct b.subscriber_id)::numeric/
    count(distinct a.subscriber_id)::numeric, 4) * 100
     as conversion_rate
from trial_subscriptions as a
left join paid_subscriptions as b
on a.subscriber_id = b.subscriber_id;
